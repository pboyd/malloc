//go:build linux || darwin || openbsd || netbsd || freebsd

package malloc

import (
	"unsafe"

	"golang.org/x/sys/unix"
)

func mmap(addr uintptr, size, prot, flags int) ([]byte, error) {
	ptr, err := unix.MmapPtr(-1, 0, unsafe.Pointer(addr), uintptr(size), unix.PROT_READ|unix.PROT_WRITE|prot, unix.MAP_ANON|unix.MAP_PRIVATE|flags)
	if err != nil {
		return nil, err
	}

	return unsafe.Slice((*byte)(unsafe.Pointer(ptr)), size), nil
}

func munmap(buf []byte) error {
	return unix.MunmapPtr(unsafe.Pointer(unsafe.SliceData(buf)), uintptr(len(buf)))
}

func mprotect(buf []byte, flags int) error {
	return unix.Mprotect(buf, flags)
}
