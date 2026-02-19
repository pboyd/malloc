//go:build linux || darwin || windows || openbsd || netbsd || freebsd

package malloc

import (
	"syscall"
	"unsafe"
)

// VirtBackend reserves a portion of virtual memory upfront and commits pages
// as needed. Only grows up to the size of its original reservation.
func VirtBackend(size uintptr, opts ...BackendOpt) (ArenaBackend, error) {
	var cfg backendConfig
	for _, opt := range opts {
		opt(&cfg)
	}

	pageSize := uintptr(syscall.Getpagesize())
	size = (size + pageSize - 1) &^ (pageSize - 1)

	ptr, err := virtReserve(cfg.addr, size, cfg.flags)
	if err != nil {
		return nil, err
	}

	return &virtBackend{
		backendConfig: cfg,
		baseAddr:      uintptr(ptr),
		capacity:      size,
	}, nil
}

type virtBackend struct {
	backendConfig

	baseAddr  uintptr
	committed uintptr
	capacity  uintptr
}

func (vb *virtBackend) Grow(_ []byte, size uintptr) ([]byte, error) {
	pageSize := uintptr(syscall.Getpagesize())
	size = (size + pageSize - 1) &^ (pageSize - 1)

	if size+vb.committed > vb.capacity {
		return nil, ErrOutOfMemory
	}

	err := virtCommit(vb.baseAddr+vb.committed, size, vb.prot)
	if err != nil {
		return nil, err
	}

	vb.committed += size

	return unsafe.Slice((*byte)(unsafe.Pointer(vb.baseAddr)), int(vb.committed)), nil
}

func (vb *virtBackend) Protect(prot int) error {
	if vb.committed == 0 {
		return nil
	}
	buf := unsafe.Slice((*byte)(unsafe.Pointer(vb.baseAddr)), int(vb.committed))
	return mprotect(buf, prot)
}
