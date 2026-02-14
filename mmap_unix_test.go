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

func TestMmapBackend_GrowFromEmpty(t *testing.T) {
	assert := assert.New(t)

	a := NewArena(48, Backend(MmapBackend(0, 0)))
	p1, err := Malloc[[80]byte](a)
	if !assert.NoError(err) {
		return
	}

	// Write some data to verify memory is usable
	for i := range p1 {
		p1[i] = byte(i)
	}

	// Verify data was written correctly
	for i := range p1 {
		assert.Equal(byte(i), p1[i])
	}

	Free(a, p1)

	// Arena should have grown to accommodate the allocation
	assert.GreaterOrEqual(a.Cap(), 80)
}

func TestMmapBackend_GrowFromFull(t *testing.T) {
	assert := assert.New(t)

	a := NewArena(64, Backend(MmapBackend(0, 0)))
	p1, err := Malloc[[64 - wordSize]byte](a)
	if !assert.NoError(err) {
		return
	}

	// Fill the first allocation with a pattern
	for i := range p1 {
		p1[i] = 0xAA
	}

	// This should trigger a grow since arena is full
	p2, err := Malloc[[64]byte](a)
	if !assert.NoError(err) {
		return
	}

	// Fill the second allocation with a different pattern
	for i := range p2 {
		p2[i] = 0xBB
	}

	// Verify both allocations maintained their data
	for i := range p1 {
		assert.Equal(byte(0xAA), p1[i], "first allocation should be unchanged")
	}
	for i := range p2 {
		assert.Equal(byte(0xBB), p2[i], "second allocation should be intact")
	}

	Free(a, p1)
	Free(a, p2)
}

func TestMmapBackend_GrowFromFragmented(t *testing.T) {
	assert := assert.New(t)

	a := NewArena(64, Backend(MmapBackend(0, 0)))
	p3, _ := a.Malloc(16)
	p2, _ := a.Malloc(16)
	p1, _ := a.Malloc(16)

	// Write patterns to each allocation
	s1 := unsafe.Slice((*byte)(p1), 16)
	s2 := unsafe.Slice((*byte)(p2), 16)
	s3 := unsafe.Slice((*byte)(p3), 16)
	for i := 0; i < 16; i++ {
		s1[i] = 0x11
		s2[i] = 0x22
		s3[i] = 0x33
	}

	// Free the middle creating a hole
	a.Free(p2, 16)

	// This allocation is too large for the hole and should trigger growth
	p4, err := a.Malloc(64)
	if !assert.NoError(err) {
		return
	}

	// Verify the remaining allocations still have correct data
	assert.Equal(byte(0x11), s1[0])
	assert.Equal(byte(0x33), s3[0])

	// Write to the new allocation
	s4 := unsafe.Slice((*byte)(p4), 64)
	for i := 0; i < 64; i++ {
		s4[i] = 0x44
	}
	assert.Equal(byte(0x44), s4[0])

	a.Free(p1, 16)
	a.Free(p3, 16)
	a.Free(p4, 64)
}

func TestMmapBackend_MultipleGrows(t *testing.T) {
	assert := assert.New(t)

	a := NewArena(32, Backend(MmapBackend(0, 0)))

	// Allocate progressively, forcing multiple grows
	allocations := make([]unsafe.Pointer, 0, 10)
	sizes := []int{16, 32, 64, 128, 256}

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
