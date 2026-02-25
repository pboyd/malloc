//go:build linux || darwin || windows || openbsd || netbsd || freebsd

package malloc

import (
	"syscall"
	"testing"
	"unsafe"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestVirtBackend_InitialAllocation(t *testing.T) {
	assert := assert.New(t)

	backend, err := VirtBackend(128 * 1024)
	require.NoError(t, err)
	defer backend.Release()

	buf, err := backend.Grow(nil, 1024)
	require.NoError(t, err)
	assert.NotNil(buf)

	// Should be at least the requested size
	assert.GreaterOrEqual(len(buf), 1024)

	// Verify memory is writable
	for i := range buf {
		buf[i] = byte(i % 256)
	}

	// Verify memory is readable
	for i := range buf {
		assert.Equal(byte(i%256), buf[i])
	}
}

func TestVirtBackend_PageAlignment(t *testing.T) {
	pageSize := syscall.Getpagesize()

	tests := []struct {
		name string
		size uintptr
	}{
		{"page size", uintptr(pageSize)},
		{"multiple pages", uintptr(pageSize) * 3},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			backend, err := VirtBackend(uintptr(pageSize) * 64)
			require.NoError(t, err)
			defer backend.Release()

			buf, err := backend.Grow(nil, tt.size)
			require.NoError(t, err)

			// Verify size is at least what was requested
			assert.GreaterOrEqual(t, len(buf), int(tt.size))

			// Verify memory is accessible
			buf[0] = 0xAB
			assert.Equal(t, byte(0xAB), buf[0])
		})
	}
}

func TestVirtBackendGrowth(t *testing.T) {
	runBackendGrowthTests(t, "VirtBackend", func() ArenaBackend {
		backend, err := VirtBackend(128 * 1024)
		if err != nil {
			panic(err)
		}
		t.Cleanup(func() { backend.Release() })
		return backend
	})
}

func TestVirtBackend_OutOfMemory(t *testing.T) {
	assert := assert.New(t)

	pageSize := uintptr(syscall.Getpagesize())

	// Reserve exactly 1 page
	backend, err := VirtBackend(pageSize)
	require.NoError(t, err)
	defer backend.Release()

	// First grow should succeed
	buf, err := backend.Grow(nil, pageSize)
	require.NoError(t, err)
	assert.NotNil(buf)

	// Second grow should fail with ErrOutOfMemory
	_, err = backend.Grow(buf, 1)
	assert.ErrorIs(err, ErrOutOfMemory)
}

func TestVirtBackend_CapacityLimit(t *testing.T) {
	assert := assert.New(t)

	pageSize := uintptr(syscall.Getpagesize())
	capacity := pageSize * 4

	backend, err := VirtBackend(capacity)
	require.NoError(t, err)
	defer backend.Release()

	// Grow to capacity
	buf, err := backend.Grow(nil, capacity)
	require.NoError(t, err)
	assert.GreaterOrEqual(len(buf), int(capacity))

	// Any further growth should fail
	_, err = backend.Grow(buf, 1)
	assert.ErrorIs(err, ErrOutOfMemory)
}

func TestVirtBackend_MultipleGrows(t *testing.T) {
	assert := assert.New(t)

	pageSize := uintptr(syscall.Getpagesize())

	backend, err := VirtBackend(pageSize * 8)
	require.NoError(t, err)
	defer backend.Release()

	// First grow
	buf, err := backend.Grow(nil, pageSize)
	require.NoError(t, err)
	assert.GreaterOrEqual(len(buf), int(pageSize))

	// Write data to verify it's accessible
	for i := range buf {
		buf[i] = byte(i % 256)
	}

	// Second grow
	buf2, err := backend.Grow(buf, pageSize)
	require.NoError(t, err)
	assert.GreaterOrEqual(len(buf2), int(pageSize)*2)

	// Original data is still intact (same base address)
	for i := 0; i < int(pageSize); i++ {
		assert.Equal(byte(i%256), buf2[i], "data from first grow should be intact")
	}

	// New memory is accessible
	s := unsafe.Slice((*byte)(unsafe.Pointer(uintptr(unsafe.Pointer(unsafe.SliceData(buf2)))+pageSize)), pageSize)
	for i := range s {
		s[i] = byte(i % 256)
	}
	for i := range s {
		assert.Equal(byte(i%256), s[i])
	}
}

func TestVirtBackend_SameBaseAddress(t *testing.T) {
	// VirtBackend grows in-place: each Grow returns a buffer starting
	// at the same base address with a larger length.
	pageSize := uintptr(syscall.Getpagesize())

	backend, err := VirtBackend(pageSize * 4)
	require.NoError(t, err)
	defer backend.Release()

	buf1, err := backend.Grow(nil, pageSize)
	require.NoError(t, err)

	buf2, err := backend.Grow(buf1, pageSize)
	require.NoError(t, err)

	// Both buffers share the same base address
	assert.Equal(t,
		uintptr(unsafe.Pointer(unsafe.SliceData(buf1))),
		uintptr(unsafe.Pointer(unsafe.SliceData(buf2))),
		"VirtBackend should return buffers with the same base address",
	)
	assert.Greater(t, len(buf2), len(buf1))
}

func TestVirtBackend_Protect_NoCommit(t *testing.T) {
	// Protect before any memory is committed should succeed (no-op).
	backend, err := VirtBackend(128 * 1024)
	require.NoError(t, err)
	defer backend.Release()

	err = backend.Protect(testProtReadWrite)
	assert.NoError(t, err, "Protect should succeed with no committed memory")
}

func TestVirtBackend_Protect_SingleRegion(t *testing.T) {
	assert := assert.New(t)

	pageSize := uintptr(syscall.Getpagesize())
	backend, err := VirtBackend(pageSize * 4)
	require.NoError(t, err)
	defer backend.Release()

	buf, err := backend.Grow(nil, pageSize)
	require.NoError(t, err)

	// Write initial data
	for i := range buf {
		buf[i] = byte(i % 256)
	}

	// Protect (keeping read-write to avoid segfault in test)
	err = backend.Protect(testProtReadWrite)
	assert.NoError(err, "Protect should succeed on committed region")

	// Data should still be readable and writable
	for i := range buf {
		assert.Equal(byte(i%256), buf[i])
	}
	buf[0] = 0xFF
	assert.Equal(byte(0xFF), buf[0])
}

func TestVirtBackend_Protect_ChangeProtection(t *testing.T) {
	assert := assert.New(t)

	pageSize := uintptr(syscall.Getpagesize())
	backend, err := VirtBackend(pageSize * 4)
	require.NoError(t, err)
	defer backend.Release()

	buf, err := backend.Grow(nil, pageSize)
	require.NoError(t, err)

	// Write initial data
	for i := range buf {
		buf[i] = byte(i % 256)
	}

	// Change to read-only
	err = backend.Protect(testProtRead)
	assert.NoError(err, "Protect should succeed when changing to read-only")

	// Data should still be readable
	for i := range buf {
		assert.Equal(byte(i%256), buf[i], "data should be readable after read-only protection")
	}

	// Change back to read-write
	err = backend.Protect(testProtReadWrite)
	assert.NoError(err, "Protect should succeed when changing to read-write")

	// Should be writable again
	buf[0] = 0x42
	assert.Equal(byte(0x42), buf[0])
}

func TestVirtBackend_Protect_CoversFullCommit(t *testing.T) {
	// After multiple grows, Protect should cover the entire committed region.
	assert := assert.New(t)

	pageSize := uintptr(syscall.Getpagesize())
	backend, err := VirtBackend(pageSize * 8)
	require.NoError(t, err)
	defer backend.Release()

	buf, err := backend.Grow(nil, pageSize)
	require.NoError(t, err)
	for i := range buf {
		buf[i] = 0x11
	}

	buf2, err := backend.Grow(buf, pageSize)
	require.NoError(t, err)

	// Write to the second page
	secondPage := buf2[pageSize:]
	for i := range secondPage {
		secondPage[i] = 0x22
	}

	// Change the entire committed region to read-only
	err = backend.Protect(testProtRead)
	assert.NoError(err)

	// Both pages should be readable
	assert.Equal(byte(0x11), buf2[0], "first page should be readable")
	assert.Equal(byte(0x22), buf2[pageSize], "second page should be readable")

	// Change back to read-write
	err = backend.Protect(testProtReadWrite)
	assert.NoError(err)

	// Both pages should be writable again
	buf2[0] = 0xAA
	buf2[pageSize] = 0xBB
	assert.Equal(byte(0xAA), buf2[0])
	assert.Equal(byte(0xBB), buf2[pageSize])
}

func TestVirtBackend_Protect_NoAccess(t *testing.T) {
	pageSize := uintptr(syscall.Getpagesize())
	backend, err := VirtBackend(pageSize * 2)
	require.NoError(t, err)
	defer backend.Release()

	buf, err := backend.Grow(nil, pageSize)
	require.NoError(t, err)

	// Set no-access protection (accessing the memory would cause a segfault,
	// but the syscall itself should succeed)
	err = backend.Protect(testProtNone)
	assert.NoError(t, err)

	// Restore to read-write so cleanup doesn't crash
	err = backend.Protect(testProtReadWrite)
	assert.NoError(t, err)

	buf[0] = 0x01
	assert.Equal(t, byte(0x01), buf[0])
}

func TestVirtBackend_Protect_ProtFlagsPassedToCommit(t *testing.T) {
	// MmapProt specifies additional flags OR'd in when committing pages.
	pageSize := uintptr(syscall.Getpagesize())
	backend, err := VirtBackend(pageSize*2, MmapProt(testProtReadWrite))
	require.NoError(t, err)
	defer backend.Release()

	buf, err := backend.Grow(nil, pageSize)
	require.NoError(t, err)
	assert.NotNil(t, buf)

	buf[0] = 0x55
	assert.Equal(t, byte(0x55), buf[0])
}

func TestVirtBackend_StressTest(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping stress test in short mode")
	}

	assert := assert.New(t)
	pageSize := syscall.Getpagesize()
	a := NewArena(uint64(pageSize), Backend(func() ArenaBackend {
		backend, err := VirtBackend(128 * 1024)
		if err != nil {
			t.Fatal(err)
		}
		t.Cleanup(func() { backend.Release() })
		return backend
	}()))

	// Allocate and free many times to test growth stability
	for iter := 0; iter < 100; iter++ {
		p, err := a.Malloc(uintptr(pageSize))
		if !assert.NoError(err) {
			return
		}

		// Write pattern
		slice := unsafe.Slice((*byte)(p), pageSize)
		for i := range slice {
			slice[i] = byte(iter % 256)
		}

		// Verify pattern
		for i := range slice {
			assert.Equal(byte(iter%256), slice[i])
		}

		a.Free(p, uintptr(pageSize))
	}
}
