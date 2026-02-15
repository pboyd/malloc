//go:build linux || darwin

package malloc

import (
	"errors"
	"syscall"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// Platform-specific memory protection constants for testing
const (
	testProtRead      = syscall.PROT_READ
	testProtWrite     = syscall.PROT_WRITE
	testProtReadWrite = syscall.PROT_READ | syscall.PROT_WRITE
	testProtNone      = syscall.PROT_NONE
)

func TestMmapBackend_ProtectionFlags(t *testing.T) {
	// Test with different protection flags
	backend := MmapBackend(syscall.PROT_EXEC, 0)
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
