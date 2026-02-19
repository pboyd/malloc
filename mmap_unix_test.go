//go:build linux || darwin || openbsd || netbsd || freebsd

package malloc

import (
	"syscall"
)

// Platform-specific memory protection constants for testing
const (
	testProtRead      = syscall.PROT_READ
	testProtWrite     = syscall.PROT_WRITE
	testProtReadWrite = syscall.PROT_READ | syscall.PROT_WRITE
	testProtNone      = syscall.PROT_NONE
	testProtExec      = syscall.PROT_EXEC
)
