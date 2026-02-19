//go:build linux || darwin || windows

package malloc

import (
	"errors"
	"math"
	"syscall"
	"testing"
	"unsafe"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestMmapBackend_InitialAllocation(t *testing.T) {
	assert := assert.New(t)

	backend := MmapBackend()
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

	backend := MmapBackend()
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
		return MmapBackend()
	})
}

func TestMmapBackend_SizeValidation(t *testing.T) {
	backend := MmapBackend()

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
	pageSize := syscall.Getpagesize()
	a := NewArena(uint64(pageSize), Backend(MmapBackend()))

	// Allocate and free many times to test growth stability
	// Each iteration allocates a full page, forcing growth on the second iteration
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

func TestMmapBackend_ProtectedArenaBackend(t *testing.T) {
	// Verify that mmapBackend implements ProtectedArenaBackend
	var _ ProtectedArenaBackend = (*mmapBackend)(nil)

	backend := MmapBackend()
	protectedBackend, ok := backend.(ProtectedArenaBackend)
	assert.True(t, ok, "MmapBackend should implement ProtectedArenaBackend")
	assert.NotNil(t, protectedBackend)
}

func TestMmapBackend_Protect_NoBuffers(t *testing.T) {
	backend := MmapBackend()
	protectedBackend := backend.(ProtectedArenaBackend)

	// Protecting when no buffers are allocated should succeed
	err := protectedBackend.Protect(testProtReadWrite)
	assert.NoError(t, err, "Protect should succeed with no active buffers")
}

func TestMmapBackend_Protect_SingleBuffer(t *testing.T) {
	backend := MmapBackend()
	protectedBackend := backend.(ProtectedArenaBackend)

	// Allocate a buffer
	buf, err := backend.Grow(nil, 1024)
	require.NoError(t, err)
	defer backend.(FreeableArenaBackend).Free(buf)

	// Write some data to verify buffer is writable
	for i := range buf {
		buf[i] = byte(i % 256)
	}

	// Protect should succeed (keeping it read-write to avoid segfault in test)
	err = protectedBackend.Protect(testProtReadWrite)
	assert.NoError(t, err, "Protect should succeed on allocated buffer")

	// Verify we can still read after protecting with read-write permissions
	for i := range buf {
		assert.Equal(t, byte(i%256), buf[i])
	}
}

func TestMmapBackend_Protect_MultipleBuffers(t *testing.T) {
	backend := MmapBackend()
	protectedBackend := backend.(ProtectedArenaBackend)

	// Allocate multiple buffers
	buf1, err := backend.Grow(nil, 1024)
	require.NoError(t, err)
	defer backend.(FreeableArenaBackend).Free(buf1)

	buf2, err := backend.Grow(nil, 2048)
	require.NoError(t, err)
	defer backend.(FreeableArenaBackend).Free(buf2)

	buf3, err := backend.Grow(nil, 512)
	require.NoError(t, err)
	defer backend.(FreeableArenaBackend).Free(buf3)

	// Write patterns to all buffers
	for i := range buf1 {
		buf1[i] = 1
	}
	for i := range buf2 {
		buf2[i] = 2
	}
	for i := range buf3 {
		buf3[i] = 3
	}

	// Protect all buffers (keeping them read-write)
	err = protectedBackend.Protect(testProtReadWrite)
	assert.NoError(t, err, "Protect should succeed on multiple buffers")

	// Verify all buffers are still accessible
	for i := range buf1 {
		assert.Equal(t, byte(1), buf1[i])
	}
	for i := range buf2 {
		assert.Equal(t, byte(2), buf2[i])
	}
	for i := range buf3 {
		assert.Equal(t, byte(3), buf3[i])
	}
}

func TestMmapBackend_Protect_AfterFree(t *testing.T) {
	backend := MmapBackend()
	protectedBackend := backend.(ProtectedArenaBackend)

	// Allocate and free a buffer
	buf, err := backend.Grow(nil, 1024)
	require.NoError(t, err)

	err = backend.(FreeableArenaBackend).Free(buf)
	require.NoError(t, err)

	// Protect after freeing should succeed (no active buffers)
	err = protectedBackend.Protect(testProtReadWrite)
	assert.NoError(t, err, "Protect should succeed after all buffers are freed")
}

func TestMmapBackend_Protect_ChangeProtection(t *testing.T) {
	backend := MmapBackend()
	protectedBackend := backend.(ProtectedArenaBackend)

	// Allocate a buffer
	buf, err := backend.Grow(nil, 4096)
	require.NoError(t, err)
	defer backend.(FreeableArenaBackend).Free(buf)

	// Write initial data
	for i := 0; i < len(buf); i++ {
		buf[i] = byte(i % 256)
	}

	// Change to read-only protection
	err = protectedBackend.Protect(testProtRead)
	assert.NoError(t, err, "Protect should succeed when changing to read-only")

	// Verify data is still readable
	for i := 0; i < len(buf); i++ {
		assert.Equal(t, byte(i%256), buf[i], "data should be readable after read-only protection")
	}

	// Change back to read-write
	err = protectedBackend.Protect(testProtReadWrite)
	assert.NoError(t, err, "Protect should succeed when changing to read-write")

	// Verify we can write again
	buf[0] = 123
	assert.Equal(t, byte(123), buf[0])
}

func TestMmapBackend_Protect_MultipleBuffersWithFlags(t *testing.T) {
	backend := MmapBackend()
	protectedBackend := backend.(ProtectedArenaBackend)

	// Allocate multiple buffers
	buf1, err := backend.Grow(nil, 4096)
	require.NoError(t, err)
	defer backend.(FreeableArenaBackend).Free(buf1)

	buf2, err := backend.Grow(nil, 4096)
	require.NoError(t, err)
	defer backend.(FreeableArenaBackend).Free(buf2)

	// Write data to both
	for i := range buf1 {
		buf1[i] = 1
	}
	for i := range buf2 {
		buf2[i] = 2
	}

	// Protect both buffers as read-only
	err = protectedBackend.Protect(testProtRead)
	assert.NoError(t, err)

	// Verify both are readable
	assert.Equal(t, byte(1), buf1[0])
	assert.Equal(t, byte(2), buf2[0])

	// Change both back to read-write
	err = protectedBackend.Protect(testProtReadWrite)
	assert.NoError(t, err)

	// Verify both are writable
	buf1[100] = 11
	buf2[100] = 22
	assert.Equal(t, byte(11), buf1[100])
	assert.Equal(t, byte(22), buf2[100])
}

func TestMmapBackend_Protect_NoAccess(t *testing.T) {
	backend := MmapBackend()
	protectedBackend := backend.(ProtectedArenaBackend)

	// Allocate a buffer
	buf, err := backend.Grow(nil, 4096)
	require.NoError(t, err)
	defer backend.(FreeableArenaBackend).Free(buf)

	// Try to set no access protection
	err = protectedBackend.Protect(testProtNone)
	// This should succeed from the system call's perspective
	// (though accessing the memory would cause a segfault/access violation)
	assert.NoError(t, err)

	// Set back to read-write so we can clean up without issues
	err = protectedBackend.Protect(testProtReadWrite)
	assert.NoError(t, err)
}

func TestMmapBackend_ProtectionFlags(t *testing.T) {
	// Test with different protection flags
	backend := MmapBackend(MmapProt(testProtExec))
	buf, err := backend.Grow(nil, 4096)
	if errors.Is(err, syscall.EPERM) || errors.Is(err, syscall.EACCES) {
		t.Skipf("Skipping test: %v", err)
	}

	require.NoError(t, err)
	defer backend.(FreeableArenaBackend).Free(buf)

	// Memory should be readable and writable (always added)
	buf[0] = 42
	assert.Equal(t, byte(42), buf[0])

	// Note: We can't easily test PROT_EXEC without writing machine code
	// but we can verify the allocation succeeds
	assert.NotNil(t, buf)
}
