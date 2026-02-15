//go:build windows

package malloc

import (
	"golang.org/x/sys/windows"
)

// Platform-specific memory protection constants for testing
const (
	testProtRead      = windows.PAGE_READONLY
	testProtWrite     = windows.PAGE_READWRITE // Windows doesn't have write-only
	testProtReadWrite = windows.PAGE_READWRITE
	testProtNone      = windows.PAGE_NOACCESS
	testProtExec      = windows.PAGE_EXECUTE
)
