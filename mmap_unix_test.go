//go:build linux || darwin

package malloc

import (
	"math"
	"syscall"
	"testing"
	"unsafe"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestMmapBackend_InitialAllocation(t *testing.T) {
	assert := assert.New(t)

	backend := MmapBackend(0, 0)
	buf, err := backend.Grow(nil, 1024)

	require.NoError(t, err)
	assert.NotNil(buf)

	// Should be at least the requested size
	assert.GreaterOrEqual(len(buf), 1024)

	// Should be page-aligned
	pageSize := syscall.Getpagesize()
	assert.Equal(0, len(buf)%pageSize, "buffer size should be page-aligned")

	// Verify memory is writable
	for i := range buf {
		buf[i] = byte(i % 256)
	}

	// Verify memory is readable
	for i := range buf {
		assert.Equal(byte(i%256), buf[i])
	}

	backend.(FreeableArenaBackend).Free(buf)
}

func TestMmapBackend_PageAlignment(t *testing.T) {
	tests := []struct {
		name string
		size uintptr
	}{
		{"small allocation", 1},
		{"page size", uintptr(syscall.Getpagesize())},
		{"page size + 1", uintptr(syscall.Getpagesize()) + 1},
		{"multiple pages", uintptr(syscall.Getpagesize()) * 3},
		{"odd size", 1337},
	}

	backend := MmapBackend(0, 0)
	pageSize := syscall.Getpagesize()

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			buf, err := backend.Grow(nil, tt.size)
			require.NoError(t, err)
			defer backend.(FreeableArenaBackend).Free(buf)

			// Verify page alignment
			assert.Equal(t, 0, len(buf)%pageSize, "buffer size should be page-aligned")

			// Verify size is at least what was requested
			assert.GreaterOrEqual(t, len(buf), int(tt.size))
		})
	}
}

func TestMmapBackendGrowth(t *testing.T) {
	runBackendGrowthTests(t, "MmapBackend", func() ArenaBackend {
		return MmapBackend(0, 0)
	})
}

func TestMmapBackend_ProtectionFlags(t *testing.T) {
	// Test with different protection flags
	backend := MmapBackend(syscall.PROT_EXEC, 0)
	buf, err := backend.Grow(nil, 4096)

	require.NoError(t, err)
	defer backend.(FreeableArenaBackend).Free(buf)

	// Memory should be readable and writable (always added)
	buf[0] = 42
	assert.Equal(t, byte(42), buf[0])

	// Note: We can't easily test PROT_EXEC without writing machine code
	// but we can verify the allocation succeeds
	assert.NotNil(t, buf)
}

func TestMmapBackend_SizeValidation(t *testing.T) {
	backend := MmapBackend(0, 0)

	// Test maximum size boundary
	_, err := backend.Grow(nil, uintptr(math.MaxInt))
	// This may succeed or fail depending on system limits
	// We're just testing that it doesn't panic
	_ = err

	// Test size larger than MaxInt
	_, err = backend.Grow(nil, uintptr(math.MaxInt)+1)
	assert.Error(t, err, "should reject size > MaxInt")
}

func TestMmapBackend_StressTest(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping stress test in short mode")
	}

	assert := assert.New(t)
	a := NewArena(1024, Backend(MmapBackend(0, 0)))

	// Allocate and free many times to test growth stability
	for iter := 0; iter < 100; iter++ {
		p, err := a.Malloc(1024)
		if !assert.NoError(err) {
			return
		}

		// Write pattern
		slice := unsafe.Slice((*byte)(p), 1024)
		for i := range slice {
			slice[i] = byte(iter % 256)
		}

		// Verify pattern
		for i := range slice {
			assert.Equal(byte(iter%256), slice[i])
		}

		a.Free(p, 1024)
	}
}
