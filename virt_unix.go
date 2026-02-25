//go:build linux || darwin || openbsd || netbsd || freebsd

package malloc

import (
	"unsafe"

	"golang.org/x/sys/unix"
)

func virtReserve(addr uintptr, size uintptr, flags int) (unsafe.Pointer, error) {
	return unix.MmapPtr(-1, 0, unsafe.Pointer(addr), size, unix.PROT_NONE, unix.MAP_ANON|unix.MAP_PRIVATE|flags)
}

func virtFree(addr uintptr, size uintptr) error {
	return unix.MunmapPtr(unsafe.Pointer(addr), size)
}

func virtCommit(addr, size uintptr, prot int) error {
	return unix.Mprotect(unsafe.Slice((*byte)(unsafe.Pointer(addr)), int(size)), unix.PROT_READ|unix.PROT_WRITE|prot)
}
