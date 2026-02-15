//go:build linux || darwin

package malloc

import (
	"golang.org/x/sys/unix"
)

func mmap(size, prot, flags int) ([]byte, error) {
	return unix.Mmap(-1, 0, size, unix.PROT_READ|unix.PROT_WRITE|prot, unix.MAP_ANON|unix.MAP_PRIVATE|flags)
}

func munmap(buf []byte) error {
	return unix.Munmap(buf)
}

func mprotect(buf []byte, flags int) error {
	return unix.Mprotect(buf, flags)
}
