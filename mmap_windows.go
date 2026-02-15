//go:build windows

package malloc

import (
	"unsafe"

	"golang.org/x/sys/windows"
)

func mmap(size, prot, flags int) ([]byte, error) {
	ptr, err := windows.VirtualAlloc(0, uintptr(size), uint32(windows.MEM_COMMIT|windows.MEM_RESERVE|flags), uint32(windows.PAGE_READWRITE|prot))
	if err != nil {
		return nil, err
	}
	return unsafe.Slice((*byte)(unsafe.Pointer(ptr)), size), nil
}

func munmap(buf []byte) error {
	ptr := uintptr(unsafe.Pointer(unsafe.SliceData(buf)))
	return windows.VirtualFree(ptr, 0, windows.MEM_RELEASE)
}

func mprotect(buf []byte, flags int) error {
	addr := uintptr(unsafe.Pointer(unsafe.SliceData(buf)))

	var oldFlags uint32
	return windows.VirtualProtect(addr, uintptr(len(buf)), uint32(flags), &oldFlags)
}
