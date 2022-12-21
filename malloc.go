package malloc

import (
	"encoding/binary"
	"errors"
	"reflect"
	"unsafe"
)

// Number of bytes in each word.
const wordSize = 16

// ErrOutOfMemory is returned when there is insufficient available memory in
// the Arena to satisfy the request.
var ErrOutOfMemory = errors.New("out of memory")

// Arena holds a fixed amount of memory which can be allocated.
type Arena struct {
	buf [][2]uint64
}

// NewArena makes a new arena of the given size.
//
// The largest theoretical size of an arena is 32 GiB.
func NewArena(size uint64) *Arena {
	words := uintptrToWords(uintptr(size))

	a := &Arena{buf: make([][2]uint64, words)}

	// Layout the initial memory:
	// - The first free block has no space, it exists to point to the first
	//   actual free block. Which is initially the second block
	// - The second block has all the space which isn't in the first block.

	second := a.index(1)
	second.size = words - 1

	first := a.index(0)
	first.next = second

	return a
}

// Size returns the total amount of memory (in bytes) managed by the arena.
func (a *Arena) Size() int {
	return len(a.buf) * wordSize
}

// Cap returns the total amount of usable memory in the arena.
func (a *Arena) Cap() int {
	return a.Size() - int(wordSize)
}

// FreeBytes returns the amount of unallocated space in the arena.
//
// This requires walking the list of free blocks, so it can be slow.
func (a *Arena) FreeBytes() int {
	freeWords := 0

	for q := a.index(0); q != nil; q = q.next {
		freeWords += int(q.size)
	}

	return freeWords * wordSize
}

// Malloc allocates a new pointer in the arena of at least the given size. Size
// is in bytes.
//
// Memory is managed in 16-byte words. If size is not evenly divisible by 16
// it will be rounded up.
func (a *Arena) Malloc(size uintptr) (unsafe.Pointer, error) {
	// This algorithm is based on the First-fit allocator in Knuth AOCP 1.2.5.

	words := uintptrToWords(size)

	// Search through the list of free blocks looking for one big enough to hold
	// the requested size.
	q := a.index(0)
	p := q.next
	for p != nil && p.size < words {
		q = p
		p = q.next
	}

	if p == nil {
		return nil, ErrOutOfMemory
	}

	// We can fit the amount requested in p, and q is block immediately before p.

	// Find the new size of the free block.
	k := p.size - words

	// Is all the space in p taken up by the new request?
	if k == 0 {
		q.next = p.next
	} else {
		p.size = k
	}

	return p.Pointer(k), nil
}

// Free deallocates size bytes of memory starting a the pointer.
//
// If the pointer is not inside the arena Free will panic.
func (a *Arena) Free(x unsafe.Pointer, size uintptr) {
	// This is the "Liberation" algorithm from Knuth 1.2.5 that goes along with
	// the first-fit algorithm above.
	//
	// The idea is to convert the referenced memory to a free node (i.e. add a
	// blockHeader) and insert it into the free list. If the block is adjacent
	// to another free block they'll be merged together.
	if x == nil {
		return
	}

	words := uintptrToWords(size)

	p0 := (*blockHeader)(x)
	if !a.Contains(p0) {
		panic("attempted to free a pointer that is not in arena")
	}

	// Search through the free list to find the entry immediately before the area
	// to be freed, or the end of the list, whichever comes first.
	//
	// This assumes that later entries in the free list have greater memory
	// addresses.

	q := a.index(0)
	p := q.next
	for p != nil && addrOf(p) < addrOf(p0) {
		q = p
		p = q.next
	}

	// Now q is the entry before p0 and p is entry after it (which will be nil,
	// if p0 is in the last block), so p0 should be inserted in the list between
	// those two entries.

	if p != nil && addrOf(p0)+uintptr(words*wordSize) == addrOf(p) {
		// Nothing was allocated between p0 and p, so merge the two free blocks together.
		words += p.size
		p0.next = p.next
	} else {
		p0.next = p
	}
	if addrOf(q)+uintptr(q.size) == addrOf(p0) {
		// Nothing was allocated between q an p0, so merge the two free blocks together.
		q.size += words
		q.next = p0.next
	} else {
		q.next = p0
		p0.size = words
	}
}

// Raw makes a copy of the memory for for debugging.
func (a *Arena) Raw() []byte {
	buf := make([]byte, len(a.buf)*wordSize)
	for i, v := range a.buf {
		offset := i * wordSize
		binary.LittleEndian.PutUint64(buf[offset:offset+8], v[0])
		offset += 8
		binary.LittleEndian.PutUint64(buf[offset:offset+8], v[1])
	}
	return buf
}

// Contains returns true if the value the pointer points to is contained in the
// arena.
//
// Panics if p is not a pointer.
func (a *Arena) Contains(p any) bool {
	addr := addrOf(p)
	return addr >= uintptr(unsafe.Pointer(&a.buf[0])) && addr <= uintptr(unsafe.Pointer(&a.buf[len(a.buf)-1]))
}

// index returns the nth blockHeader, if the entire memory were an array of
// blockHeaders. This is generally unsafe, except that index 0 is always the
// list head. And in an empty arena index 1 has all the space.
func (a *Arena) index(n uint32) *blockHeader {
	return (*blockHeader)(unsafe.Pointer(&a.buf[n]))
}

// Malloc allocates a pointer of an arbitrary type in the arena.
func Malloc[T any](a *Arena) (*T, error) {
	p, err := a.Malloc(unsafe.Sizeof((*(*T)(nil))))
	if err != nil {
		return nil, err
	}
	return (*T)(p), nil
}

// Free deallocates the memory associated with a pointer that was previously
// allocated in the arena.
//
// Free will panic if the pointer was not allocated within the arena.
func Free[T any](a *Arena, p *T) {
	a.Free(unsafe.Pointer(p), unsafe.Sizeof((*(*T)(nil))))
}

func uintptrToWords(s uintptr) uint32 {
	if s%wordSize == 0 {
		return uint32(s / wordSize)
	}

	return uint32(s/wordSize + 1)
}

// blockHeader marks the beginning of each free block.
type blockHeader struct {
	// next is a pointer to the header for the next free block.
	next *blockHeader

	// size is the number of free words in the block. This header is considered
	// free space, so it's counted in size.
	size uint32
}

// Pointer returns an unsafe.Pointer to the nth word in the block.
func (b *blockHeader) Pointer(n uint32) unsafe.Pointer {
	return unsafe.Pointer(uintptr(unsafe.Pointer(b)) + uintptr(n*wordSize))
}

func addrOf(p any) uintptr {
	return uintptr(reflect.ValueOf(p).UnsafePointer())
}
