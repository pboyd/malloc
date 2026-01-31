package malloc

import (
	"fmt"
	"math"
	"math/rand"
	"testing"
	"unsafe"

	"github.com/stretchr/testify/assert"
)

func TestMallocAll(t *testing.T) {
	assert := assert.New(t)

	// Allocate everything in the arena (after the header) in a single array.
	a := NewArena(64)
	p, err := Malloc[[64 - wordSize]byte](a)
	if !assert.NoError(err) {
		return
	}

	// Set every value in the pointer and ensure the raw memory dump matches
	// exactly.
	for i := 0; i < len(*p); i++ {
		(*p)[i] = byte(i)
	}
	for i, b := range a.Raw()[wordSize:] {
		assert.Equal(byte(i), b)
	}

	// Verify that no more memory can be allocated.
	overflow, err := Malloc[byte](a)
	assert.Nil(overflow)
	assert.ErrorIs(err, ErrOutOfMemory)

	Free(a, p)
	assert.Equal(a.FreeBytes(), a.Cap())
}

func TestMallocSmallItems(t *testing.T) {
	assert := assert.New(t)

	a := NewArena(10 * 1024)

	allocAll := func() []*uint64 {
		// It's really innefficient for this algorithm to store 8-byte values,
		// because the smallest value it can handle is 16 bytes. So we end up
		// wasting over half the space.
		pointers := make([]*uint64, a.Cap()/16)
		for i := range pointers {
			var err error
			pointers[i], err = Malloc[uint64](a)
			if !assert.NoError(err) {
				return pointers
			}
			*pointers[i] = ^uint64(0)
		}

		overflow, err := Malloc[byte](a)
		assert.Nil(overflow)
		assert.ErrorIs(err, ErrOutOfMemory)

		return pointers
	}

	// Allocate everything and make sure it's full.
	pointers := allocAll()
	assert.Equal(a.FreeBytes(), 0)

	// Now free everything.
	for _, p := range pointers {
		Free(a, p)
	}

	assert.Equal(a.FreeBytes(), a.Cap())

	// Now allocate it all again.
	pointers = allocAll()
	assert.Equal(a.FreeBytes(), 0)

	// Free it all again, but this free it in a random order.
	rnd := rand.New(rand.NewSource(1))
	for len(pointers) > 0 {
		n := rnd.Intn(len(pointers))
		p := pointers[n]
		pointers = append(pointers[:n], pointers[n+1:]...)
		Free(a, p)
	}

	assert.Equal(a.FreeBytes(), a.Cap())
}

func TestMallocStruct(t *testing.T) {
	assert := assert.New(t)

	type Arbitrary struct {
		Field1 int64
		Field2 float64
		Field3 [16]byte
	}

	a := NewArena(128)
	p, err := Malloc[Arbitrary](a)
	if !assert.NoError(err) {
		return
	}

	assert.True(a.Contains(p))
}

func TestMallocFunction(t *testing.T) {
	assert := assert.New(t)
	type IntFunc func(int) int

	a := NewArena(128)
	double, err := Malloc[IntFunc](a)
	if !assert.NoError(err) {
		return
	}

	*double = func(x int) int {
		return x * 2
	}

	assert.True(a.Contains(double))
	assert.Equal(4, (*double)(2))
}

func TestMallocZero(t *testing.T) {
	assert := assert.New(t)

	a := NewArena(64)
	p, err := a.Malloc(0)
	assert.Nil(p)
	assert.NoError(err)

	// Verify the arena is unaffected
	assert.Equal(48, a.FreeBytes())
}

func TestMallocSlice(t *testing.T) {
	assert := assert.New(t)

	a := NewArena(128)
	initialFree := a.FreeBytes()

	// len=0, cap=100
	buf, err := MallocSlice[byte](a, 0, 100)
	if !assert.NoError(err) {
		return
	}
	assert.Equal(0, len(buf))
	assert.Equal(100, cap(buf))
	FreeSlice(a, buf)

	// len=100, cap=len
	buf, err = MallocSlice[byte](a, 100)
	if !assert.NoError(err) {
		return
	}
	assert.Equal(100, len(buf))
	assert.Equal(100, cap(buf))

	_, err = MallocSlice[int64](a, 0, 10)
	assert.ErrorIs(err, ErrOutOfMemory)

	FreeSlice(a, buf)

	intSlice, err := MallocSlice[int64](a, 5, 10)
	if !assert.NoError(err) {
		return
	}
	assert.Equal(10, cap(intSlice))

	// Slice data should be in the arena after appending up to capacity
	intSlice = append(intSlice, 6, 7, 8, 9, 10)
	assert.Equal([]int64{0, 0, 0, 0, 0, 6, 7, 8, 9, 10}, intSlice)
	assert.True(a.Contains(unsafe.SliceData(intSlice)))

	// Slice data is moved from the arena if the slice grows beyond capacity
	newIntSlice := append(intSlice, 11)
	assert.False(a.Contains(unsafe.SliceData(newIntSlice)))

	// But it's possible to still use and free the original slice
	assert.True(a.Contains(unsafe.SliceData(intSlice)))
	assert.Equal(10, len(intSlice))
	assert.Equal(10, cap(intSlice))
	FreeSlice(a, intSlice)
	assert.Equal(initialFree, a.FreeBytes())
}

func TestMallocSliceIsZerod(t *testing.T) {
	assert := assert.New(t)

	a := NewArena(64)
	buf, err := MallocSlice[byte](a, 48)
	if !assert.NoError(err) {
		return
	}

	for i := range buf {
		buf[i] = 0xff
	}
	FreeSlice(a, buf)

	newBuf, err := MallocSlice[byte](a, 48)
	if !assert.NoError(err) {
		return
	}

	sum := 0
	for _, c := range newBuf {
		sum += int(c)
	}
	assert.Equal(0, sum)
}

func TestMallocObjectIsZerod(t *testing.T) {
	assert := assert.New(t)

	a := NewArena(64)
	buf, err := MallocSlice[byte](a, 48)
	if !assert.NoError(err) {
		return
	}

	for i := range buf {
		buf[i] = 0xff
	}
	FreeSlice(a, buf)

	ptr, err := Malloc[uint](a)
	if assert.NoError(err) {
		assert.Equal(uint(0), *ptr)
	}

}

func TestRandomMallocs(t *testing.T) {
	assert := assert.New(t)

	ra := newRandomAllocator(rand.NewSource(0), 10*1024)

	assert.Equal(ra.Arena.FreeBytes(), ra.Arena.Cap())

	for i := 1; i <= 10000; i++ {
		switch {
		case i%2 == 0:
			ra.Malloc(64)
		case i%5 == 0:
			ra.Free(1)
		case i%100 == 0:
			ra.Malloc(10000)
		}
	}

	ra.FreeAll()
	assert.Equal(ra.Arena.FreeBytes(), ra.Arena.Cap())
}

func BenchmarkMallocAndFree(b *testing.B) {
	assert := assert.New(b)
	ra := newRandomAllocator(rand.NewSource(0), 1024*1024)
	b.ResetTimer()

	for i := 1; i <= b.N; i++ {
		switch {
		case i%2 == 0:
			ra.Malloc(64)
		case i%5 == 0:
			ra.Free(1)
		case i%100 == 0:
			ra.Malloc(10000)
		}
	}

	ra.FreeAll()
	assert.Equal(ra.Arena.FreeBytes(), ra.Arena.Cap())
}

func TestNewArenaAt(t *testing.T) {
	assert := assert.New(t)

	buf := make([]byte, 0, 100)
	arena := NewArenaAt(buf)
	assert.Equal(6, len(arena.buf), "arena size should round down to an even word size")
}

func TestArenaSizes(t *testing.T) {
	assert := assert.New(t)

	// Rounds up to 2 words
	arena := NewArena(17)
	assert.NotNil(arena)
	assert.Equal(2, len(arena.buf))

	// Too small to fit the initial free block headers
	arena = NewArena(16)
	assert.Nil(arena)

	// Theoretically this package could support up to 64 GiB. Not sure how
	// well it would work up near the top of that range, but we at least to
	// prevent uint32 overflows.
	arena = NewArena(math.MaxUint32 + 1)
	assert.Nil(arena)
}

func TestBackwardMerge(t *testing.T) {
	assert := assert.New(t)

	// Create arena with exactly 4 words (64 bytes)
	// Word 0: header, Words 1-3: free
	a := NewArena(64)

	// Sanity check: 48 free bytes which can be allocated in one chunk
	assert.Equal(48, a.FreeBytes())
	p, _ := a.Malloc(48)
	assert.Equal(0, a.FreeBytes())
	a.Free(p, 48)

	// Allocate 16 bytes. It should be allocated as the very last word
	// Word 0: header, Words 1-2: free, Word 3: this pointer
	p, _ = a.Malloc(16)
	assert.Equal(32, a.FreeBytes())

	// Now free the buffer and it should merge with the block before it and
	// be exactly the same as the initial arena
	// Word 0: header, Words 1-3: free
	a.Free(p, 16)
	assert.Equal(48, a.FreeBytes())

	// However there was a bug where backward merges didn't work:
	// Word 0: header, Words 1-2: free block, Word 3: separate free block
	// So this call would fail because of fragmentation even though all the space was available.
	_, err := a.Malloc(48)
	assert.NoError(err)
	assert.Equal(0, a.FreeBytes())
}

func TestDoubleFree(t *testing.T) {
	t.Run("should panic when freeing the start of a free block", func(t *testing.T) {
		a := NewArena(32)
		p, _ := a.Malloc(16)

		// First free
		a.Free(p, 16)

		// Second free should panic
		err := func() (err error) {
			defer func() {
				if r := recover(); r != nil {
					err = fmt.Errorf("caught panic: %v", r)
				}
			}()
			a.Free(p, 16)
			return
		}()

		assert.ErrorContains(t, err, "double-free")
	})

	t.Run("should panic when freeing inside a free block", func(t *testing.T) {
		a := NewArena(48)
		p2, _ := a.Malloc(16)
		p1, _ := a.Malloc(16)

		// Word 0: header, Word 1: p1, Word 2: p2

		a.Free(p1, 16)
		a.Free(p2, 16)

		// Now everything is unallocated, but we still have a pointer
		// inside the free block
		// Word 0: header, Words 1-2: free (but p1 and p2 still point here)

		// Freeing p2 again should panic
		err := func() (err error) {
			defer func() {
				if r := recover(); r != nil {
					err = fmt.Errorf("caught panic: %v", r)
				}
			}()
			a.Free(p2, 16)
			return
		}()

		assert.ErrorContains(t, err, "double-free")
		assert.Equal(t, 32, a.FreeBytes())
	})

	t.Run("double-free inside another allocated block", func(t *testing.T) {
		// This serves to document the sort of double-free bugs that aren't detected.

		a := NewArena(64)
		p3, _ := a.Malloc(16)
		p2, _ := a.Malloc(16)
		p1, _ := a.Malloc(16)

		// Word 0: header, Word 1: p1, Word 2: p2, Word 3: p3

		a.Free(p1, 16)
		a.Free(p2, 16)
		a.Free(p3, 16)

		// Everything is unallocated, but the pointers still exist.
		// Get a new block that takes everything.
		p, _ := a.Malloc(48)

		// Free p2 again, which is right in the middle of the other allocation.
		a.Free(p2, 16)

		// Now we have a real mess. According to the arena:
		//     Word 0: header, Word 1: reserved, Word 2: free, Word 3: reserved
		//
		// The owner of p believes it has all 48 bytes, but it's
		// corrupted with our block header in the middle.
		//
		// If for some reason the program doesn't crash after this,
		// freeing the larger block should get back to a good state.
		a.Free(p, 48)
		assert.Equal(t, 48, a.FreeBytes())
	})
}

type randomAllocator struct {
	rnd     *rand.Rand
	records []allocRecord
	Arena   *Arena
}

type allocRecord struct {
	pointer unsafe.Pointer
	size    uintptr
}

func newRandomAllocator(source rand.Source, arenaSize uint64) *randomAllocator {
	return &randomAllocator{
		rnd:     rand.New(source),
		records: []allocRecord{},
		Arena:   NewArena(arenaSize),
	}
}

func (ra *randomAllocator) Malloc(max int) bool {
	size := uintptr(ra.rnd.Intn(max-1) + 1)
	p, err := ra.Arena.Malloc(size)
	if err != nil {
		return false
	}

	ra.records = append(ra.records, allocRecord{
		pointer: p,
		size:    size,
	})

	return true
}

// Free takes n random entries from the arena and frees them.
func (ra *randomAllocator) Free(n int) {
	for ; n > 0 && len(ra.records) > 0; n-- {
		i := ra.rnd.Intn(len(ra.records))
		ra.Arena.Free(ra.records[i].pointer, ra.records[i].size)
		ra.records = append(ra.records[:i], ra.records[i+1:]...)
	}
}

func (ra *randomAllocator) FreeAll() {
	for _, rec := range ra.records {
		ra.Arena.Free(rec.pointer, rec.size)
	}
	ra.records = ra.records[:0]
}
