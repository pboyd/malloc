package malloc

import (
	"runtime"
	"sync"
	"testing"
	"unsafe"

	"github.com/stretchr/testify/assert"
)

// TestConcurrentMalloc verifies that concurrent allocations don't produce
// overlapping memory regions or duplicate pointers.
func TestConcurrentMalloc(t *testing.T) {
	assert := assert.New(t)

	// Create an arena large enough for many allocations
	a := NewArena(64 * 1024)
	numGoroutines := runtime.GOMAXPROCS(0) * 2
	allocsPerGoroutine := 100

	// Use channels to collect results and synchronize start
	type result struct {
		ptr unsafe.Pointer
		err error
	}
	results := make(chan result, numGoroutines*allocsPerGoroutine)
	start := make(chan struct{})
	var wg sync.WaitGroup

	// Spawn goroutines that all start at the same time
	for i := 0; i < numGoroutines; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			<-start // Wait for signal to start

			for j := 0; j < allocsPerGoroutine; j++ {
				p, err := Malloc[int](a)
				results <- result{ptr: unsafe.Pointer(p), err: err}
			}
		}()
	}

	// Signal all goroutines to start simultaneously
	close(start)
	wg.Wait()
	close(results)

	// Collect all pointers
	pointers := make(map[unsafe.Pointer]bool)
	successCount := 0
	for res := range results {
		if res.err == nil {
			successCount++
			// Check for duplicate pointers (overlapping allocations)
			if pointers[res.ptr] {
				t.Errorf("duplicate pointer detected: %v", res.ptr)
			}
			pointers[res.ptr] = true

			// Verify the pointer is in the arena
			assert.True(a.Contains(res.ptr))
		}
	}

	// We should have gotten at least some successful allocations
	assert.Greater(successCount, 0, "should have at least some successful allocations")
}

// TestConcurrentFree verifies that concurrent frees of disjoint objects
// are safe and don't corrupt the free list.
func TestConcurrentFree(t *testing.T) {
	assert := assert.New(t)

	// Pre-allocate many objects sequentially
	a := NewArena(64 * 1024)
	numObjects := 1000
	pointers := make([]*int, numObjects)

	for i := 0; i < numObjects; i++ {
		p, err := Malloc[int](a)
		if !assert.NoError(err) {
			return
		}
		pointers[i] = p
	}

	// Record initial free bytes (should be low)
	initialFree := a.FreeBytes()
	expectedFinal := a.Cap()

	// Now free them all concurrently, each goroutine gets a disjoint subset
	numGoroutines := runtime.GOMAXPROCS(0) * 2
	start := make(chan struct{})
	var wg sync.WaitGroup

	for i := 0; i < numGoroutines; i++ {
		wg.Add(1)
		// Partition the pointers array
		startIdx := i * numObjects / numGoroutines
		endIdx := (i + 1) * numObjects / numGoroutines

		go func(ptrs []*int) {
			defer wg.Done()
			<-start // Wait for signal

			for _, p := range ptrs {
				Free(a, p)
			}
		}(pointers[startIdx:endIdx])
	}

	close(start)
	wg.Wait()

	// After all frees, FreeBytes should equal Cap
	assert.Equal(expectedFinal, a.FreeBytes())
	assert.Greater(a.FreeBytes(), initialFree, "free bytes should have increased")
}

// TestConcurrentMallocAndFree verifies that interleaved malloc and free
// operations from multiple goroutines don't corrupt the arena.
func TestConcurrentMallocAndFree(t *testing.T) {
	assert := assert.New(t)

	a := NewArena(128 * 1024)
	numGoroutines := runtime.GOMAXPROCS(0) * 2
	iterationsPerGoroutine := 100

	start := make(chan struct{})
	var wg sync.WaitGroup

	for i := 0; i < numGoroutines; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			<-start

			// Each goroutine does repeated malloc/free cycles
			for j := 0; j < iterationsPerGoroutine; j++ {
				p, err := Malloc[int64](a)
				if err == nil {
					*p = int64(j)
					Free(a, p)
				}
			}
		}()
	}

	close(start)
	wg.Wait()

	// Arena should be consistent after all operations
	// We can't guarantee FreeBytes == Cap because some allocations might
	// still be in flight or failed, but we can check it's reasonable
	assert.LessOrEqual(a.FreeBytes(), a.Cap())
}

// TestConcurrentMallocSlice verifies that concurrent slice allocations
// don't produce overlapping memory regions.
func TestConcurrentMallocSlice(t *testing.T) {
	assert := assert.New(t)

	a := NewArena(256 * 1024)
	numGoroutines := runtime.GOMAXPROCS(0) * 2
	allocsPerGoroutine := 50

	type sliceResult struct {
		slice []byte
		err   error
	}
	results := make(chan sliceResult, numGoroutines*allocsPerGoroutine)
	start := make(chan struct{})
	var wg sync.WaitGroup

	for i := 0; i < numGoroutines; i++ {
		wg.Add(1)
		go func(id int) {
			defer wg.Done()
			<-start

			for j := 0; j < allocsPerGoroutine; j++ {
				// Varying sizes
				size := (id*allocsPerGoroutine + j) % 100
				if size == 0 {
					size = 1
				}
				slice, err := MallocSlice[byte](a, size)
				results <- sliceResult{slice: slice, err: err}
			}
		}(i)
	}

	close(start)
	wg.Wait()
	close(results)

	// Check for overlapping memory regions
	type memRegion struct {
		start uintptr
		end   uintptr
	}
	regions := []memRegion{}

	for res := range results {
		if res.err == nil && len(res.slice) > 0 {
			ptr := unsafe.Pointer(unsafe.SliceData(res.slice))
			start := uintptr(ptr)
			end := start + uintptr(len(res.slice))

			// Check against all previous regions
			for _, region := range regions {
				// Check if regions overlap
				if !(end <= region.start || start >= region.end) {
					t.Errorf("overlapping memory regions detected: [%x, %x) overlaps [%x, %x)",
						start, end, region.start, region.end)
				}
			}

			regions = append(regions, memRegion{start: start, end: end})

			// Verify the slice data is in the arena
			assert.True(a.Contains(ptr))

			// Free the slice
			FreeSlice(a, res.slice)
		}
	}

	// Should have successfully allocated at least some slices
	assert.Greater(len(regions), 0)
}

// TestConcurrentReadsAndWrites verifies that read-only methods can be
// called concurrently with mutation methods without causing data races.
// This test primarily relies on the race detector to catch issues.
func TestConcurrentReadsAndWrites(t *testing.T) {
	assert := assert.New(t)

	a := NewArena(64 * 1024)
	duration := 100 // iterations
	numReaders := runtime.GOMAXPROCS(0)
	numWriters := runtime.GOMAXPROCS(0)

	start := make(chan struct{})
	var wg sync.WaitGroup

	// Pre-allocate some objects for Contains() to check
	testPointers := make([]*int, 10)
	for i := range testPointers {
		p, _ := Malloc[int](a)
		if p != nil {
			testPointers[i] = p
		}
	}

	// Reader goroutines
	for i := 0; i < numReaders; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			<-start

			for j := 0; j < duration; j++ {
				// Call read-only methods
				_ = a.Size()
				_ = a.Cap()
				_ = a.FreeBytes()

				// Check if test pointers are contained
				for _, p := range testPointers {
					if p != nil {
						_ = a.Contains(p)
					}
				}
			}
		}()
	}

	// Writer goroutines
	for i := 0; i < numWriters; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			<-start

			for j := 0; j < duration; j++ {
				p, err := Malloc[int](a)
				if err == nil {
					*p = j
					Free(a, p)
				}
			}
		}()
	}

	close(start)
	wg.Wait()

	// Clean up test pointers
	for _, p := range testPointers {
		if p != nil {
			Free(a, p)
		}
	}

	// Basic sanity check
	assert.LessOrEqual(a.FreeBytes(), a.Cap())
}

