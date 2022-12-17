package malloc

import (
	"encoding/binary"
	"errors"
	"fmt"
	"reflect"
	"unsafe"
)

// Number of bytes in each word. This is the smallest size we will allocate.
const wordSize = 8

// The size of blockHeader in words.
const blockHeaderSize = unsafe.Sizeof((*(*blockHeader)(nil))) / wordSize

// ErrOutOfMemory is returned when there is insufficient available memory in
// the Arena to satisfy the request.
var ErrOutOfMemory = errors.New("out of memory")

// Arena holds a fixed amount of memory which can be allocated.
type Arena struct {
	buf  []uint64
	head blockHeader
}

// NewArena makes a new arena of the given size.
//
// NewArena will panic if size is not a multiple of 8.
func NewArena(size uintptr) *Arena {
	if size%wordSize != 0 {
		panic(fmt.Sprintf("size must be a multiple of %d", wordSize))
	}

	words := uint32(size / wordSize)

	buf := make([]uint64, words)

	// Mark the first block as having all the space.
	first := (*blockHeader)(unsafe.Pointer(&buf[0]))
	first.next = nil
	first.size = words

	return &Arena{
		buf:  buf,
		head: blockHeader{next: first},
	}
}

// Malloc allocates a new pointer in the arena of at least the given size. Size
// is in bytes.
//
// Memory is allocated in 8-byte chunks. If size is not evenly divisible by 8
// it will be rounded up.
func (a *Arena) Malloc(size uintptr) (unsafe.Pointer, error) {
	var words uint32
	if size%wordSize == 0 {
		words = uint32(size / wordSize)
	} else {
		words = uint32(size/wordSize + 1)
	}

	// This algorithm is based on the First-fit allocator in Knuth AOCP 1.2.5.

	q := &a.head
	p := q.next

	for p != nil {
		if p.size < words {
			// Doesn't fit, move on.
			q = p
			p = q.next
			continue
		}

		// We can fit the
		k := p.size - words

		// Knuth says to check if k == 0. But since our header doesn't fit in one
		// word, we'd end up giving part of our header away.
		if uintptr(k) < blockHeaderSize {
			q.next = p.next
		} else {
			p.size = k
		}

		return p.Pointer(k), nil
	}

	return nil, ErrOutOfMemory
}

func (a *Arena) Raw() []byte {
	buf := make([]byte, len(a.buf)*wordSize)
	for i, v := range a.buf {
		binary.LittleEndian.PutUint64(buf[i*8:i*8+8], v)
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

// Malloc allocates a pointer of an arbitrary type in the arena.
func Malloc[T any](a *Arena) (*T, error) {
	p, err := a.Malloc(unsafe.Sizeof((*(*T)(nil))))
	if err != nil {
		return nil, err
	}
	return (*T)(p), nil
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
