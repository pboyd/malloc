//go:build windows

package malloc

import (
	"unsafe"

	"golang.org/x/sys/windows"
)

func virtReserve(addr uintptr, size uintptr, flags int) (unsafe.Pointer, error) {
	ptr, err := windows.VirtualAlloc(addr, size, uint32(windows.MEM_RESERVE|flags), windows.PAGE_NOACCESS)
	if err != nil {
		return nil, err
	}
	return unsafe.Pointer(ptr), nil
}

func virtFree(addr uintptr, size uintptr) error {
	return windows.VirtualFree(addr, 0, windows.MEM_RELEASE)
}

func virtCommit(addr, size uintptr, prot int) error {
	if prot&windows.PAGE_EXECUTE != 0 {
		prot = windows.PAGE_EXECUTE_READWRITE | (prot ^ windows.PAGE_EXECUTE)
	} else {
		prot = windows.PAGE_READWRITE | prot
	}

	_, err := windows.VirtualAlloc(addr, size, windows.MEM_COMMIT, uint32(prot))
	return err
}
