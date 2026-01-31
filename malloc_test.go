package malloc

import (
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
