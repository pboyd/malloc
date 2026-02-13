package malloc

import (
	"errors"
	"fmt"
	"log"
	"math"
	"reflect"
	"slices"
	"unsafe"

	"golang.org/x/exp/constraints"
)

// Number of bytes in each word.
const wordSize = 16

// ErrOutOfMemory is returned when there is insufficient available memory in
// the Arena to satisfy the request.
var ErrOutOfMemory = errors.New("out of memory")

// Opt is a configuration option passed to NewArena or NewArenaAt.
type Opt func(*Arena)

// Backend is used to manage the underlying memory for the arena.
func Backend(backend ArenaBackend) Opt {
	return func(a *Arena) {
		a.backend = backend
	}
}

// Arena holds an amount of memory which can be allocated.
//
// Arena is not safe for concurrent use. Use a mutex if it will be used in
// multiple goroutines.
type Arena struct {
	buf     [][2]uint64
	backend ArenaBackend

	// archive contains old versions of buf from before an expansion which
	// still contain allocated memory. When all pointers have been freed
	// these will be removed.
	archive [][][2]uint64
}

// NewArena makes a new arena of the given size. If the size is not evenly
// divisible by the word size (16 bytes) it will be rounded up.
//
// Size (after rounding) must be between 32 bytes and 64 GiB (theoretically).
// NewArena returns nil if size does not fall in that range.
//
// The arena requires 16 bytes for the initial block header, so the maximum
// allocatable size in the arena will be size - 16 bytes.
//
// By default, NewArena manages a fixed amount of memory obtained from a
// standard Go slice.
func NewArena(size uint64, opts ...Opt) *Arena {
	if size%wordSize != 0 {
		size += wordSize - size%wordSize
	}
	if size > math.MaxUint32 {
		return nil
	}

	arena := &Arena{
		backend: fixedBackend{},
	}
	for _, opt := range opts {
		opt(arena)
	}

	buf, err := arena.backend.Grow(nil, uintptr(size))
	if err != nil {
		return nil
	}
	arena.buf = byteSliceToInternal(buf)

	return arena.init()
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
func NewArenaAt[T any](buf []T, opts ...Opt) *Arena {
	addr := unsafe.SliceData(buf)
	if addr == nil {
		return nil
	}

	arena := &Arena{
		backend: fixedBackend{},
	}
	for _, opt := range opts {
		opt(arena)
	}

	words := uintptr(cap(buf)) * unsafe.Sizeof(*addr) / wordSize
	arena.buf = unsafe.Slice((*[2]uint64)(unsafe.Pointer(addr)), words)
	return arena.init()
}

func (a *Arena) init() *Arena {
	words := len(a.buf)
	if words < 2 {
		// Not enough space for even the initial free blocks.
		return nil
	}

	// 64 GiB is the largest theoretical size supported. Cap the buffer to
	// that if necessary.
	if words > math.MaxUint32/wordSize {
		words = math.MaxUint32/wordSize - 1
	}

	// Layout the initial memory:
	// - The first free block has no space, it exists to point to the first
	//   actual free block. Which is initially the second block
	// - The second block has all the space which isn't in the first block.

	second := index(a.buf, 1)
	second.size = uint32(words) - 1

	first := index(a.buf, 0)
	first.next = second

	return a
}

// Size returns the amount of memory (in bytes) managed by the arena. Note that
// this does not include the size of old memory areas that are preserved after
// the arena grows.
func (a *Arena) Size() int {
	return len(a.buf) * wordSize
}

// Cap returns the total amount of allocatable memory in the arena.
func (a *Arena) Cap() int {
	return a.Size() - int(wordSize)
}

// FreeBytes returns the amount of allocatable space in the arena.
//
// This requires walking the list of free blocks, so it can be slow.
func (a *Arena) FreeBytes() int {
	freeWords := 0

	for q := index(a.buf, 0); q != nil; q = q.next {
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
	q := index(a.buf, 0)
	p := q.next
	for p != nil && p.size < words {
		q = p
		p = q.next
	}

	if p == nil {
		// No space left, attempt to grow the buffer.
		err := a.grow(size)
		if err != nil {
			return nil, err
		}
		return a.Malloc(size)
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

func (a *Arena) grow(size uintptr) error {
	originalLen := uint32(len(a.buf))
	originalEnd := uintptr(unsafe.Pointer(unsafe.SliceData(a.buf))) + (uintptr(len(a.buf)) * wordSize)

	buf, err := a.backend.Grow(internalToByteSlice(a.buf), size)
	if err != nil {
		return err
	}

	isNewBuf := unsafe.Pointer(unsafe.SliceData(a.buf)) != unsafe.Pointer(unsafe.SliceData(buf))
	if isNewBuf {
		// We got a new buffer instead of just a larger version of the
		// old one. If it's empty, then we'll free it right now.
		// Otherwise, we need to keep a copy to it so that we can free
		// the memory we've already allocated there.
		if isEmpty(a.buf) {
			if freeableBackend, ok := a.backend.(FreeableArenaBackend); ok {
				freeableBackend.Free(internalToByteSlice(a.buf))
			}
		} else {
			// Keep a copy of the old buffer. We won't allocate anything
			// new there, but there are still pointers to it.
			a.archive = append(a.archive, a.buf)
		}
	}

	a.buf = byteSliceToInternal(buf)

	if !isNewBuf {
		// Find the last free block
		lastFreeBlock := index(a.buf, 0)
		p := lastFreeBlock.next
		for p != nil {
			lastFreeBlock = p
			p = lastFreeBlock.next
		}

		if uintptr(unsafe.Pointer(lastFreeBlock))+uintptr(lastFreeBlock.size)*wordSize == originalEnd {
			// The last free block went right up to the end of the
			// old buffer, so increase that free block with all the
			// words in the new space.
			newEnd := uintptr(unsafe.Pointer(unsafe.SliceData(a.buf))) + (uintptr(len(a.buf)) * wordSize)
			lastFreeBlock.size += uint32((newEnd - originalEnd) / wordSize)
		} else {
			// Insert a new free block and point to it.
			newBlock := index(a.buf, originalLen)
			newBlock.size = uint32(len(a.buf)) - originalLen
			lastFreeBlock.next = newBlock
		}
	} else {
		a.init()
	}

	return nil
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

	p := (*byte)(x)

	// First check if we have a pointer to our active buffer
	if contains(p, a.buf) {
		a.freeFromBuf(x, size, a.buf)
		return
	}

	// The pointer was not to the active buffer, so look for it in one of the other buffers
	for i, oldBuf := range a.archive {
		if contains(p, oldBuf) {
			a.freeFromBuf(x, size, oldBuf)

			// If that was the last pointer in the archive buffer
			// remove the reference to it.
			if isEmpty(oldBuf) {
				if freeableBackend, ok := a.backend.(FreeableArenaBackend); ok {
					freeableBackend.Free(internalToByteSlice(oldBuf))
				}

				// This delete would cause problems with the
				// loop, but we're returning anyway.
				a.archive = slices.Delete(a.archive, i, i+1)
			}

			return
		}
	}

	// It's not a pointer to anything managed by this package.
	panic("attempted to free a pointer that is not in arena")

}

func (*Arena) freeFromBuf(x unsafe.Pointer, size uintptr, buf [][2]uint64) {
	// Search through the free list to find the entries immediately before
	// and after the area to be freed, or the end of the list, whichever
	// comes first.
	//
	// This assumes that later entries in the free list have greater memory
	// addresses.

	words := uintptrToWords(size)

	p := (*blockHeader)(x)

	before := index(buf, 0)
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
	copy(buf, internalToByteSlice(a.buf))
	return buf
}

// Contains returns true if the value the pointer points to is contained in the
// arena.
//
// Panics if p is not a pointer.
func (a *Arena) Contains(p any) bool {
	addr := uintptr(reflect.ValueOf(p).UnsafePointer())

	// Check the active buffer
	if containsAddr(addr, a.buf) {
		return true
	}

	// Check archived buffers
	for _, oldBuf := range a.archive {
		if containsAddr(addr, oldBuf) {
			return true
		}
	}

	return false
}

// index assumes that the entire buffer is filled with blockHeaders and returns
// the nth one. This is generally unsafe, except for index 0 and 1.
func index(buf [][2]uint64, n uint32) *blockHeader {
	return (*blockHeader)(unsafe.Pointer(&buf[n]))
}

func isEmpty(buf [][2]uint64) bool {
	first := index(buf, 0)
	if first.size == 0 {
		second := first.next
		if second != nil && second.size == uint32(len(buf)-1) {
			return true
		}
	}
	return false
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
	return containsAddr(addrOf(ptr), buf)
}

func addrOf[T any](p *T) uintptr {
	return uintptr(unsafe.Pointer(p))
}

func containsAddr(addr uintptr, buf [][2]uint64) bool {
	return addr >= uintptr(unsafe.Pointer(&buf[0])) && addr <= uintptr(unsafe.Pointer(&buf[len(buf)-1]))
}

func byteSliceToInternal(buf []byte) [][2]uint64 {
	return unsafe.Slice((*[2]uint64)(unsafe.Pointer(unsafe.SliceData(buf))), len(buf)>>4)
}

func internalToByteSlice(buf [][2]uint64) []byte {
	return unsafe.Slice((*byte)(unsafe.Pointer(unsafe.SliceData(buf))), len(buf)<<4)
}
