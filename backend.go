package malloc

// ArenaBackend is the interface to allocate the underlying memory for an Arena.
type ArenaBackend interface {
	// Grow is called when an Arena requires more memory.
	//
	// size is the minimum amount of new space to add. The returned buffer
	// must be at least len(buf)+size bytes, but Grow may return a larger
	// buffer than requested.
	//
	// If the backend cannot provide at least size additional bytes, Grow
	// must return ErrOutOfMemory.
	//
	// buf will be nil the first time Grow is called. Ideally, Grow returns
	// a larger slice with the same starting address as buf, but if
	// necessary a new buffer can be returned.
	//
	// If a new buffer is returned it does not need to contain the original
	// contents of buf.
	Grow(buf []byte, size uintptr) ([]byte, error)
}

// FreeableArenaBackend adds a method to a backend to free the underlying
// memory when all pointers have been unallocated.
//
// This is necessary when Grow is not able to extend the existing buffer and
// returns a new one instead.
type FreeableArenaBackend interface {
	// Free is called to unallocate a buffer that was originally allocated
	// by Grow. It is only used when Grow returns a new buffer and all
	// pointers to the old buffer have been removed.
	Free(buf []byte) error
}

// ProtectedArenaBackend is an optional interface for ArenaBackend (currently
// only supported by MmapBackend) to change the memory protections on the
// allocated memory.
type ProtectedArenaBackend interface {
	// Protect changes the memory protections on currently allocated pages.
	//
	// prot is passed through the underlying system call and the exact
	// value is platform-specific.
	//
	// This should be used with great care. Failure to do so will lead to
	// segmentation faults.
	Protect(prot int) error
}

type fixedBackend struct{}

func (fixedBackend) Grow(buf []byte, size uintptr) ([]byte, error) {
	if buf == nil {
		return make([]byte, size), nil
	}
	return nil, ErrOutOfMemory
}

// SliceBackend is an ArenaBackend that allocates memory using a Go slice.
var SliceBackend ArenaBackend = sliceBackend{}

type sliceBackend struct{}

func (sliceBackend) Grow(buf []byte, size uintptr) ([]byte, error) {
	return append(buf, make([]byte, size)...), nil
}
