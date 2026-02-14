// TODO: This should work for other BSD systems too, figure out which ones and update this tag:

//go:build linux || darwin

package malloc

import (
	"fmt"
	"math"
	"syscall"
)

// MmapBackend returns an ArenaBackend that allocates memory via mmap(2). This
// is only available when compiling for Linux and Darwin.
//
// prot and flags are OR'd with the defaults required and passed through to
// mmap.
func MmapBackend(prot int, flags int) ArenaBackend {
	return &mmapBackend{
		prot:  prot,
		flags: flags,
	}
}

type mmapBackend struct {
	prot  int
	flags int
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
		// Attempt to use mremap. Most systems don't have mremap, and
		// it may not work even on those that do.
		newBuf, err := mremap(buf, int(size))
		if err != nil {
			return nil, fmt.Errorf("mremap: %w", err)
		}
		if newBuf != nil {
			return newBuf, nil
		}
	}

	return mmap(int(size), mb.prot, mb.flags)
}

func (mb *mmapBackend) Free(buf []byte) error {
	return munmap(buf)
}
