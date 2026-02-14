//go:build linux || darwin

package malloc

import (
	"syscall"

	"golang.org/x/sys/unix"
)

func mmap(size, prot, flags int) ([]byte, error) {
	return unix.Mmap(-1, 0, size, syscall.PROT_READ|syscall.PROT_WRITE|prot, syscall.MAP_ANON|syscall.MAP_PRIVATE|flags)
}

func munmap(buf []byte) error {
	return unix.Munmap(buf)
}
