// TODO: This should work for other BSD systems too, figure out which ones and update this tag:

//go:build linux || darwin || windows || openbsd || netbsd || freebsd

package malloc

import (
	"errors"
	"fmt"
	"math"
	"sync"
	"syscall"
	"unsafe"
)

// MmapBackend returns an ArenaBackend that allocates memory via mmap(2) (or
// VirtualAlloc on Windows). This is tested on Linux, Darwin and Windows. It is
// confirmed to compile on some other BSD platforms, but is otherwise untested.
//
// prot and flags are OR'd with the required defaults and passed through to
// underlying system calls. The supported values are platform-specific.
func MmapBackend(prot int, flags int) ArenaBackend {
	return &mmapBackend{
		prot:   prot,
		flags:  flags,
		active: map[*byte][]byte{},
	}
}

type mmapBackend struct {
	prot  int
	flags int

	activeMu sync.Mutex
	active   map[*byte][]byte
}

var _ ProtectedArenaBackend = (*mmapBackend)(nil)

func (mb *mmapBackend) Grow(buf []byte, size uintptr) ([]byte, error) {
	pageSize := uintptr(syscall.Getpagesize())

	// The new size will be size plus the length of the old buffer, rounded
	// up to cover a full page.
	size += uintptr(len(buf))
	size = (size + pageSize - 1) &^ (pageSize - 1)

	if size > math.MaxInt {
		// This would be very odd, but catch it early.
		return nil, fmt.Errorf("invalid size")
	}

	if buf != nil {
		// Attempt to use mremap. Most systems don't have mremap, and
		// it may not work even on those that do.
		//
		// Note that this mremap wrapper doesn't pass MEM_MAYMOVE, so
		// this will either return an enlarged version of buf or fail.
		newBuf, err := mremap(buf, int(size))
		if err != nil {
			return nil, fmt.Errorf("mremap: %w", err)
		}
		if newBuf != nil {
			mb.trackBuffer(newBuf)
			return newBuf, nil
		}
	}

	newBuf, err := mmap(int(size), mb.prot, mb.flags)
	if err != nil {
		return nil, err
	}

	mb.trackBuffer(newBuf)
	return newBuf, nil
}

func (mb *mmapBackend) trackBuffer(buf []byte) {
	mb.activeMu.Lock()
	defer mb.activeMu.Unlock()
	mb.active[unsafe.SliceData(buf)] = buf
}

func (mb *mmapBackend) Free(buf []byte) error {
	addr := unsafe.SliceData(buf)

	err := munmap(buf)
	if err != nil {
		return err
	}

	mb.activeMu.Lock()
	defer mb.activeMu.Unlock()
	delete(mb.active, addr)

	return nil
}

func (mb *mmapBackend) Protect(prot int) error {
	mb.activeMu.Lock()
	defer mb.activeMu.Unlock()

	errs := make([]error, 0, len(mb.active))
	for _, buf := range mb.active {
		errs = append(errs, mprotect(buf, prot))
	}
	return errors.Join(errs...)
}
