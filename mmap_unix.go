//go:build linux || darwin

package malloc

import (
	"fmt"
	"log"
	"math"
	"syscall"
	"unsafe"
)

// MmapBackend returns an ArenaBackend that allocates memory via mmap(2).
//
// prot specifies the protections on allocated pages and supports the same
// values as mmap for your system. PROT_READ and PROT_WRITE are required so
// they will be added to the value provided.
//
// flags is also passed through to mmap. MAP_ANON and MAP_PRIVATE are always
// added to the value provided.
func MmapBackend(prot int, flags int) ArenaBackend {
	return &mmapBackend{
		prot:       prot | syscall.PROT_READ | syscall.PROT_WRITE,
		flags:      flags | syscall.MAP_ANON | syscall.MAP_PRIVATE,
		extensions: map[uintptr][][]byte{},
	}
}

type mmapBackend struct {
	prot  int
	flags int

	extensions map[uintptr][][]byte
}

func (mb *mmapBackend) Grow(buf []byte, size uintptr) ([]byte, error) {
	pageSize := uintptr(syscall.Getpagesize())

	// The new size will be the old size plus the length of the buffer,
	// rounded up to cover a full page.
	size += uintptr(len(buf))
	size = (size + pageSize - 1) &^ (pageSize - 1)

	if size > math.MaxInt {
		// This would be very odd, but catch it early.
		return nil, fmt.Errorf("invalid size")
	}

	if buf != nil {
		newBuf, ext, err := mremap(buf, int(size), mb.prot, mb.flags)
		if err != nil {
			return nil, fmt.Errorf("mremap: %w", err)
		}
		if newBuf != nil {
			if ext != nil {
				addr := uintptr(unsafe.Pointer(unsafe.SliceData(newBuf)))
				mb.extensions[addr] = append(mb.extensions[addr], ext)
			}
			return newBuf, nil
		}
	}

	return syscall.Mmap(-1, 0, int(size), mb.prot, mb.flags)
}

func (mb *mmapBackend) Free(buf []byte) {
	// If the buffer being freed was grown by combining multiple contiguous
	// mmap segements then we also need to unmap those extensions.
	addr := uintptr(unsafe.Pointer(unsafe.SliceData(buf)))
	for _, ext := range mb.extensions[addr] {
		err := syscall.Munmap(ext)
		if err != nil {
			log.Printf("munmap: %v", err)
		}
	}
	delete(mb.extensions, addr)

	err := syscall.Munmap(buf)
	if err != nil {
		log.Printf("munmap: %v", err)
	}
}
