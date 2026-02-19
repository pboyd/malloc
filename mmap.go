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

// BackendOpt is an optional parameter for MmapBackend or VirtualBackend.
type BackendOpt func(*backendConfig)

type backendConfig struct {
	flags int
	prot  int
	addr  uintptr
}

// MmapFlags specifies additional flags to pass through the underlying system call.
func MmapFlags(flags int) BackendOpt {
	return func(cfg *backendConfig) {
		cfg.flags = flags
	}
}

// MmapProt specified additional protection flags to pass through to the
// underlying system call.
//
// On Unix-like systems, this value will be OR'd with PROT_READ and PROT_WRITE.
//
// On Windows, PAGE_READWRITE is the default. If PAGE_EXECUTE is specified
// the passed flag will be PAGE_EXECUTE_READWRITE.
func MmapProt(prot int) BackendOpt {
	return func(cfg *backendConfig) {
		cfg.prot = prot
	}
}

// MmapAddr specifies the address to pass through to the underlying system call.
//
// If necessary, the address will rounded down to nearest page boundary.
func MmapAddr(addr uintptr) BackendOpt {
	addr = addr &^ uintptr(syscall.Getpagesize()-1)

	return func(cfg *backendConfig) {
		cfg.addr = addr
	}
}

// MmapBackend returns an ArenaBackend that allocates memory via mmap(2) (or
// VirtualAlloc on Windows). This is tested on Linux, Darwin and Windows. It is
// confirmed to compile for some BSD variants, but is otherwise untested.
//
// prot and flags are OR'd with the required defaults and passed through to
// underlying system calls. The supported values are platform-specific.
func MmapBackend(opts ...BackendOpt) ArenaBackend {
	mb := &mmapBackend{
		active: map[*byte][]byte{},
	}
	for _, opt := range opts {
		opt(&mb.backendConfig)
	}
	return mb
}

type mmapBackend struct {
	backendConfig

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

	newBuf, err := mmap(mb.addr, int(size), mb.prot, mb.flags)
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
