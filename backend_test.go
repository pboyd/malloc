package malloc

import (
	"math/rand"
	"os"
	"runtime"
	"sync"
	"testing"
	"unsafe"

	"github.com/stretchr/testify/assert"
)

// runBackendGrowthTests runs a standard suite of growth-related tests against
// any ArenaBackend implementation. The factory function should return a fresh
// backend instance for each subtest.
func runBackendGrowthTests(t *testing.T, backendName string, factory func() ArenaBackend) {
	pageSize := os.Getpagesize()

	t.Run(backendName, func(t *testing.T) {
		t.Run("GrowFromFull", func(t *testing.T) {
			assert := assert.New(t)

			a := NewArena(uint64(pageSize), Backend(factory()))
			// First allocation fills exactly one page
			p1, err := a.Malloc(uintptr(pageSize - wordSize))
			if !assert.NoError(err) {
				return
			}

			// Fill the first allocation with a pattern
			s1 := unsafe.Slice((*byte)(p1), pageSize-wordSize)
			for i := range s1 {
				s1[i] = 0xAA
			}

			// This should trigger a grow since arena is full
			p2, err := a.Malloc(uintptr(pageSize))
			if !assert.NoError(err) {
				return
			}

			// Fill the second allocation with a different pattern
			s2 := unsafe.Slice((*byte)(p2), pageSize)
			for i := range s2 {
				s2[i] = 0xBB
			}

			// Verify both allocations maintained their data
			for i := range s1 {
				assert.Equal(byte(0xAA), s1[i], "first allocation should be unchanged")
			}
			for i := range s2 {
				assert.Equal(byte(0xBB), s2[i], "second allocation should be intact")
			}

			a.Free(p1, uintptr(pageSize-wordSize))
			a.Free(p2, uintptr(pageSize))
		})

		t.Run("GrowFromEmpty", func(t *testing.T) {
			assert := assert.New(t)

			a := NewArena(uint64(pageSize), Backend(factory()))
			// Single allocation exceeds one page
			size := pageSize * 2
			p1, err := a.Malloc(uintptr(size))
			if !assert.NoError(err) {
				return
			}

			// Write some data to verify memory is usable
			s1 := unsafe.Slice((*byte)(p1), size)
			for i := range s1 {
				s1[i] = byte(i % 256)
			}

			// Verify data was written correctly
			for i := range s1 {
				assert.Equal(byte(i%256), s1[i])
			}

			a.Free(p1, uintptr(size))

			// Arena should have grown to accommodate the allocation
			assert.GreaterOrEqual(a.Cap(), size)
		})

		t.Run("GrowFromFragmented", func(t *testing.T) {
			assert := assert.New(t)

			a := NewArena(uint64(pageSize), Backend(factory()))
			smallSize := pageSize / 4
			p3, _ := a.Malloc(uintptr(smallSize))
			p2, _ := a.Malloc(uintptr(smallSize))
			p1, _ := a.Malloc(uintptr(smallSize))

			// Write patterns to each allocation
			s1 := unsafe.Slice((*byte)(p1), smallSize)
			s2 := unsafe.Slice((*byte)(p2), smallSize)
			s3 := unsafe.Slice((*byte)(p3), smallSize)
			for i := 0; i < smallSize; i++ {
				s1[i] = 0x11
				s2[i] = 0x22
				s3[i] = 0x33
			}

			// Free the middle creating a hole
			a.Free(p2, uintptr(smallSize))

			// This allocation is too large for the hole and should trigger growth
			p4, err := a.Malloc(uintptr(pageSize))
			if !assert.NoError(err) {
				return
			}

			// Verify the remaining allocations still have correct data
			assert.Equal(byte(0x11), s1[0])
			assert.Equal(byte(0x33), s3[0])

			// Write to the new allocation
			s4 := unsafe.Slice((*byte)(p4), pageSize)
			for i := 0; i < pageSize; i++ {
				s4[i] = 0x44
			}
			assert.Equal(byte(0x44), s4[0])

			a.Free(p1, uintptr(smallSize))
			a.Free(p3, uintptr(smallSize))
			a.Free(p4, uintptr(pageSize))
		})

		t.Run("MultipleGrows", func(t *testing.T) {
			assert := assert.New(t)

			a := NewArena(uint64(pageSize), Backend(factory()))

			// Allocate progressively, forcing multiple grows
			// Cumulative size is pageSize*3.75, guaranteeing multiple growths
			allocations := make([]unsafe.Pointer, 0, 10)
			sizes := []int{pageSize / 4, pageSize / 2, pageSize, pageSize * 2}

			for _, size := range sizes {
				p, err := a.Malloc(uintptr(size))
				if !assert.NoError(err) {
					return
				}
				allocations = append(allocations, p)

				// Fill with a pattern based on size
				pattern := byte(size % 256)
				slice := unsafe.Slice((*byte)(p), size)
				for i := 0; i < size; i++ {
					slice[i] = pattern
				}
			}

			// Verify all allocations still have correct data
			for i, p := range allocations {
				size := sizes[i]
				pattern := byte(size % 256)
				slice := unsafe.Slice((*byte)(p), size)
				for j := 0; j < size; j++ {
					assert.Equal(pattern, slice[j], "allocation %d corrupted", i)
				}
			}

			// Free all
			for i, p := range allocations {
				a.Free(p, uintptr(sizes[i]))
			}
		})

		t.Run("GrowWithRandomMallocs", func(t *testing.T) {
			assert := assert.New(t)

			a := NewArena(10*1024, Backend(factory()))
			ra := &randomAllocator{
				rnd:     rand.New(rand.NewSource(0)),
				records: []allocRecord{},
				Arena:   a,
			}

			assert.Equal(ra.Arena.FreeBytes(), ra.Arena.Cap())

			for i := 1; i <= 1000; i++ {
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
		})

		t.Run("ContainsWithArchive", func(t *testing.T) {
			assert := assert.New(t)

			a := NewArena(uint64(pageSize), Backend(factory()))

			// Allocate a pointer in the first buffer
			size1 := pageSize / 2
			p1, err := a.Malloc(uintptr(size1))
			if !assert.NoError(err) {
				return
			}
			assert.True(a.Contains(p1), "p1 should be contained in active buffer")

			// Grow the arena, which may create a new buffer and archive the old one
			size2 := pageSize
			p2, err := a.Malloc(uintptr(size2))
			if !assert.NoError(err) {
				return
			}
			assert.True(a.Contains(p2), "p2 should be contained in active buffer")

			// Verify that p1 is still reported as contained
			assert.True(a.Contains(p1), "p1 should still be contained")

			// Clean up
			a.Free(p1, uintptr(size1))
			a.Free(p2, uintptr(size2))
		})

		t.Run("ConcurrentGrow", func(t *testing.T) {
			assert := assert.New(t)

			// Start with a small arena that can grow
			a := NewArena(1024, Backend(factory()))
			initialCap := a.Cap()

			numGoroutines := runtime.GOMAXPROCS(0) * 2
			allocsPerGoroutine := 50

			type allocResult struct {
				ptr unsafe.Pointer
				err error
			}
			results := make(chan allocResult, numGoroutines*allocsPerGoroutine)
			start := make(chan struct{})
			var wg sync.WaitGroup

			for i := 0; i < numGoroutines; i++ {
				wg.Add(1)
				go func() {
					defer wg.Done()
					<-start

					// Allocate larger objects to trigger growth
					for j := 0; j < allocsPerGoroutine; j++ {
						p, err := Malloc[[64]byte](a)
						results <- allocResult{ptr: unsafe.Pointer(p), err: err}
					}
				}()
			}

			close(start)
			wg.Wait()
			close(results)

			// Collect all successful allocations
			pointers := make(map[unsafe.Pointer]bool)
			successCount := 0

			for res := range results {
				if res.err == nil {
					successCount++

					// Check for duplicate pointers
					if pointers[res.ptr] {
						t.Errorf("duplicate pointer detected after grow: %v", res.ptr)
					}
					pointers[res.ptr] = true

					// Verify pointer is contained (either in main buffer or archive)
					assert.True(a.Contains(res.ptr))
				}
			}

			// Arena should have grown (unless all allocations fit in initial capacity)
			if successCount > initialCap/64 {
				assert.Greater(a.Cap(), initialCap, "arena should have grown when allocations exceed initial capacity")
			}

			// Should have many successful allocations
			assert.Greater(successCount, allocsPerGoroutine, "should have many successful allocations")
		})
	})
}

func TestSliceBackend(t *testing.T) {
	assert := assert.New(t)

	a := NewArena(64, Backend(SliceBackend))
	p1, err := Malloc[[64 - wordSize]byte](a)
	if !assert.NoError(err) {
		return
	}

	p2, err := Malloc[[64]byte](a)
	if !assert.NoError(err) {
		return
	}

	Free(a, p1)
	assert.Len(a.archive, 0, "archive should be cleared when the last pointer is freed")

	Free(a, p2)
	assert.Equal(128-wordSize, a.Cap())
}

type growingSliceBackend struct {
	s []byte
}

func (e *growingSliceBackend) Grow(buf []byte, size uintptr) ([]byte, error) {
	newSize := len(buf) + int(size)
	if newSize > cap(e.s) {
		return nil, ErrOutOfMemory
	}
	return e.s[:newSize], nil
}

func TestGrowingBackend(t *testing.T) {
	runBackendGrowthTests(t, "growingSliceBackend", func() ArenaBackend {
		return &growingSliceBackend{s: make([]byte, 128*1024)}
	})
}

func TestSliceBackendGrowth(t *testing.T) {
	// SliceBackend has some limitations with rapid growth, so we run
	// a subset of the growth tests with adjusted parameters
	t.Run("SliceBackend", func(t *testing.T) {
		factory := func() ArenaBackend { return SliceBackend }

		t.Run("GrowFromFull", func(t *testing.T) {
			assert := assert.New(t)

			a := NewArena(64, Backend(factory()))
			p1, err := Malloc[[64 - wordSize]byte](a)
			if !assert.NoError(err) {
				return
			}

			for i := range p1 {
				p1[i] = 0xAA
			}

			p2, err := Malloc[[64]byte](a)
			if !assert.NoError(err) {
				return
			}

			for i := range p2 {
				p2[i] = 0xBB
			}

			for i := range p1 {
				assert.Equal(byte(0xAA), p1[i], "first allocation should be unchanged")
			}
			for i := range p2 {
				assert.Equal(byte(0xBB), p2[i], "second allocation should be intact")
			}

			Free(a, p1)
			Free(a, p2)
		})

		t.Run("GrowFromEmpty", func(t *testing.T) {
			assert := assert.New(t)

			a := NewArena(48, Backend(factory()))
			p1, err := Malloc[[80]byte](a)
			if !assert.NoError(err) {
				return
			}

			for i := range p1 {
				p1[i] = byte(i)
			}

			for i := range p1 {
				assert.Equal(byte(i), p1[i])
			}

			Free(a, p1)
			assert.GreaterOrEqual(a.Cap(), 80)
		})

		t.Run("ContainsWithArchive", func(t *testing.T) {
			assert := assert.New(t)

			a := NewArena(64, Backend(factory()))

			p1, err := Malloc[[32]byte](a)
			if !assert.NoError(err) {
				return
			}
			assert.True(a.Contains(p1), "p1 should be contained in active buffer")

			p2, err := Malloc[[64]byte](a)
			if !assert.NoError(err) {
				return
			}
			assert.True(a.Contains(p2), "p2 should be contained in active buffer")

			assert.True(a.Contains(p1), "p1 should still be contained")

			Free(a, p1)
			Free(a, p2)
		})
	})
}
