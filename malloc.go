package malloc

import (
	"encoding/binary"
	"errors"
	"fmt"
	"log"
	"math"
	"reflect"
	"unsafe"

	"golang.org/x/exp/constraints"
)

// Number of bytes in each word.
const wordSize = 16

// ErrOutOfMemory is returned when there is insufficient available memory in
// the Arena to satisfy the request.
var ErrOutOfMemory = errors.New("out of memory")

// Arena holds a fixed amount of memory which can be allocated.
//
// Arena is not safe for concurrent use. Use a mutex if it will be used in
// multiple goroutines.
type Arena struct {
	buf [][2]uint64
}

// NewArena makes a new arena of the given size. If the size is not evenly
// divisible by the word size (16 bytes) it will be rounded up.
//
// Size (after rounding) must be between 32 bytes and 64 GiB (theoretically).
// NewArena returns nil if size does not fall in that range.
//
// The arena requires 16 bytes for the initial block header, so the maximum
// allocatable size in the arena will be size - 16 bytes.
func NewArena(size uint64) *Arena {
	if size%wordSize != 0 {
		size += wordSize - size%wordSize
	}
	if size > math.MaxUint32 {
		return nil
	}

	return NewArenaAt(make([]byte, size))
}

// NewArenaAt creates a new Arena that will allocate memory inside the given
// buffer. Some space will be wasted if the capacity of the buffer is not
// evenly divisible by the word size (16 bytes).
//
// The type of the slice is irrelevant for NewArenaAt because it merely uses it
// as a fixed-size buffer. For convenience NewArenaAt will accept a slice of
// any type, however the data stored in the buffer will have no relation to the
// type.
//
// The caller should not modify buf directly after passing it to NewArenaAt.
func NewArenaAt[T any](buf []T) *Arena {
	addr := unsafe.SliceData(buf)
	if addr == nil {
		return nil
	}

	words := uintptr(cap(buf)) * unsafe.Sizeof(*addr) / wordSize
	if words < 2 {
		// Not enough space for even the initial free blocks.
		return nil
	}

	// 64 GiB is the largest theoretical size supported. Cap the buffer to
	// that if necessary.
	if words > math.MaxUint32/wordSize {
		words = math.MaxUint32/wordSize - 1
	}

	a := &Arena{
		buf: unsafe.Slice((*[2]uint64)(unsafe.Pointer(addr)), words),
	}

	// Layout the initial memory:
	// - The first free block has no space, it exists to point to the first
	//   actual free block. Which is initially the second block
	// - The second block has all the space which isn't in the first block.

	second := a.index(1)
	second.size = uint32(words) - 1

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
// is in bytes. Unlike the typical behavior in Go, the data returned by Malloc
// is not zeroed.
//
// Memory is managed in 16-byte words. If size is not evenly divisible by 16
// it will be rounded up.
func (a *Arena) Malloc(size uintptr) (unsafe.Pointer, error) {
	// This algorithm is based on the First-fit allocator in Knuth AOCP 1.2.5.

	if size == 0 {
		return nil, nil
	}

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

// Free deallocates a specific number of bytes of memory starting at the
// pointer. The memory range should not be used after calling Free.
//
// It's expected that most callers will free entire blocks obtained from
// Malloc, and therefore size will be rounded up to the nearest word size to
// match the behavior of Malloc. However nothing prevents freeing part of a
// block as long as the address and size are aligned to 16-bit word boundaries.
//
// If the pointer is not inside the arena Free will panic. Free will also panic
// if it detects an attempt to free already freed memory.
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

	// Verify that the address is on a word boundary.
	if uintptr(x) != uintptr(x)&^(uintptr(wordSize)-1) {
		panic(fmt.Sprintf("attempted to free non-word aligned pointer: %v", x))
	}

	words := uintptrToWords(size)

	p := (*blockHeader)(x)
	if !contains(p, a.buf) {
		panic("attempted to free a pointer that is not in arena")
	}

	// Search through the free list to find the entries immediately before
	// and after the area to be freed, or the end of the list, whichever
	// comes first.
	//
	// This assumes that later entries in the free list have greater memory
	// addresses.

	before := a.index(0)
	after := before.next
	for after != nil && addrOf(after) < addrOf(p) {
		before = after
		after = before.next
	}

	// Now p is between "before" and "after" (which will be nil, if p is
	// in the last block), so p should be inserted in the list between
	// those two entries.

	if after == p {
		// Instead of finding the block header for the previous block
		// we found the address we're trying to free. Panic because the
		// alternative would set p.next to p and cause a hang the next
		// time we walk the list.
		panic("double-free detected: attempted to free already free block")
	}

	beforeEnd := addrOf(before) + uintptr(before.size)*wordSize
	if beforeEnd > addrOf(p) {
		// The end of the block that supposedly precedes this one
		// overlaps with the space we're trying to free.
		panic("double-free detected: attempted to free inside of an already free block")
	}

	pEnd := addrOf(p) + uintptr(words)*wordSize
	if after != nil && pEnd > addrOf(after) {
		// The end of the block to free overlaps the next block. If our
		// callers were well behaved this shouldn't happen, but a
		// possible cause is:
		//
		// - func1 allocates memory and frees it
		// - func2 allocates a larger block of memory that includes the range previously allocated to func1
		// - func1 mistakenly frees its memory again
		// - func2 frees its memory
		//
		// We could panic here, but the corruption has already
		// occurred, so it seems better to consume the overlapping
		// blocks.

		log.Printf("malloc.Free: possible double-free detected: block to free overlaps another free block")

		for after != nil && pEnd > addrOf(after) {
			after = after.next
		}
	}

	if after != nil && pEnd == addrOf(after) {
		// Nothing was allocated between p and after, so merge the two free blocks together.
		words += after.size
		p.next = after.next
	} else {
		p.next = after
	}

	if beforeEnd == addrOf(p) {
		// Nothing was allocated between before and p, so merge the two free blocks together.
		before.size += words
		before.next = p.next
	} else {
		before.next = p
		p.size = words
	}
}

// Raw makes a copy of the memory for debugging.
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
	addr := uintptr(reflect.ValueOf(p).UnsafePointer())
	return addr >= uintptr(unsafe.Pointer(&a.buf[0])) && addr <= uintptr(unsafe.Pointer(&a.buf[len(a.buf)-1]))
}

// index assumes that the entire buffer is filled with blockHeaders and returns
// the nth one. This is generally unsafe, except for index 0 and 1.
func (a *Arena) index(n uint32) *blockHeader {
	return (*blockHeader)(unsafe.Pointer(&a.buf[n]))
}

// Malloc allocates a pointer of an arbitrary type in the arena.
func Malloc[T any](a *Arena) (*T, error) {
	size := sizeof[T]()
	p, err := a.Malloc(size)
	if err != nil {
		return nil, err
	}

	// Clear the memory before returning it.
	clear(unsafe.Slice((*byte)(unsafe.Pointer(p)), size))

	return (*T)(p), nil
}

// MallocSlice returns a new slice of the requested type, length and capacity.
// The slice's data will reside in the arena, but the slice header is a
// standard heap-allocated Go slice.
//
//	// Get a []int, len=10, cap=10:
//	intSlice, err := MallocSlice[int](a, 10)
//
//	// Get a []uint8, len=1, cap=100:
//	uint8Slice, err := MallocSlice[uint8](a, 1, 100)
//
// The builtin append function can be used with the slice, however if slice is
// grown beyond the allocated array it will convert to a standard Go slice and
// the memory will not be freed from the arena.
//
// MallocSlice is variadic so that capacity can be optional. If not specified,
// capacity will be equal to length. It will panic if more than one value is
// given for capacity.
//
// It will also panic if length or capacity is less than 0, or if length is
// greater than capacity.
func MallocSlice[T any, N constraints.Integer](a *Arena, length N, capacity ...N) ([]T, error) {
	if length < 0 {
		panic("malloc.MallocSlice: invalid argument: length < 0")
	}

	var c uintptr
	switch len(capacity) {
	case 0:
		c = uintptr(length)
	case 1:
		if capacity[0] < 0 {
			panic("malloc.MallocSlice: invalid argument: capacity < 0")
		}
		c = uintptr(capacity[0])
	default:
		panic("malloc.MallocSlice: multiple values provided for capacity")
	}

	if uintptr(length) > c {
		panic("malloc.MallocSlice: invalid arguments: length > capacity")
	}

	if c == 0 {
		return []T{}, nil
	}

	addr, err := a.Malloc(sizeof[T]() * uintptr(c))
	if err != nil {
		return nil, err
	}

	s := unsafe.Slice((*T)(unsafe.Pointer(addr)), c)
	clear(s)
	return s[:length], nil
}

// Free deallocates the memory associated with a pointer that was previously
// allocated in the arena.
//
// Free will panic if the pointer was not allocated within the arena.
//
// The object should not be used after calling Free.
func Free[T any](a *Arena, p *T) {
	if p == nil {
		return
	}

	a.Free(unsafe.Pointer(p), sizeof[T]())
}

// FreeSlice deallocates the data in a slice allocated with MallocSlice. This
// will panic if the slice data is not in the arena.
//
// The slice should not be used after calling Free.
func FreeSlice[T any](a *Arena, s []T) {
	if s == nil || cap(s) == 0 {
		return
	}

	a.Free(unsafe.Pointer(unsafe.SliceData(s)), sizeof[T]()*uintptr(cap(s)))
}

// ShrinkSlice frees any unused capacity in a slice. The returned slice will
// use the same underlying data as the original slice, but with a capacity
// equal to the length of the original slice.
//
// If the slice has a length of zero the entire slice is freed and an empty
// slice with no capacity is returned.
//
// Appending to the original slice after calling ShrinkSlice will corrupt the
// arena. Callers should take care to use the returned slice in place of the
// original one.
//
// This will panic if the slice data was not allocated in the arena.
func ShrinkSlice[T any](a *Arena, s []T) []T {
	if s == nil {
		return nil
	}

	elemSize := sizeof[T]()
	dataAddr := uintptr(unsafe.Pointer(unsafe.SliceData(s)))

	// Find the address of the word starting immediately after the end of
	// the data
	p := dataAddr + elemSize*(uintptr(len(s)))
	p = (p + wordSize - 1) &^ (wordSize - 1)

	// Find the end of the slice. Round it up to the next word like Malloc
	// did originally.
	sliceEnd := dataAddr + elemSize*uintptr(cap(s))
	sliceEnd = (sliceEnd + wordSize - 1) &^ (wordSize - 1)

	if sliceEnd-p >= wordSize {
		a.Free(unsafe.Pointer(p), sliceEnd-p)
	}

	return unsafe.Slice((*T)(unsafe.Pointer(dataAddr)), len(s))
}

func sizeof[T any]() uintptr {
	return unsafe.Sizeof((*(*T)(nil)))
}

// Finds the minimum number of words required to hold a value of the given
// number of bytes.
func uintptrToWords(s uintptr) uint32 {
	// Assumes wordSize is 16
	return uint32(((s + 0xf) &^ 0xf) >> 4)
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

// contains returns true if the pointer is contained inside the provided
// buffer.
func contains[T any](ptr *T, buf [][2]uint64) bool {
	addr := addrOf(ptr)
	return addr >= uintptr(unsafe.Pointer(&buf[0])) && addr <= uintptr(unsafe.Pointer(&buf[len(buf)-1]))
}

func addrOf[T any](p *T) uintptr {
	return uintptr(unsafe.Pointer(p))
}
