package malloc

// ArenaExpander is the interface to allocate the underlying memory for an Arena.
type ArenaExpander interface {
	// Grow is called when an Arena requires more memory.
	//
	// size is the minimum amount of new space to add, but Grow may return
	// a larger buffer than requested.
	//
	// If no more space is available, Grow returns ErrOutOfMemory.
	//
	// buf will be nil the first time Grow is called. Ideally, Grow returns
	// a larger slice with the same starting address as buf, but if
	// necessary a new buffer can be returned.
	//
	// If a new buffer is returned it does not need to contain the original
	// contents of buf.
	Grow(buf []byte, size uintptr) ([]byte, error)
}

// FreeableArenaExpander adds a method to an expander to free the underlying
// memory when all pointers have been unallocated.
//
// This is necessary when Grow is not able to extend the existing buffer and
// returns a new one instead.
type FreeableArenaExpander interface {
	// Free is called to unallocate a buffer that was originally allocated
	// by Grow. It is only used when Grow returns a new buffer and all
	// pointers to the old buffer have been removed.
	Free(buf []byte)
}

type fixedExpander struct{}

func (fixedExpander) Grow(buf []byte, size uintptr) ([]byte, error) {
	if buf == nil {
		return make([]byte, size), nil
	}
	return nil, ErrOutOfMemory
}

// SliceExpander is an ArenaExpander that allocates memory using a Go slice.
var SliceExpander ArenaExpander = sliceExpander{}

type sliceExpander struct{}

func (sliceExpander) Grow(buf []byte, size uintptr) ([]byte, error) {
	return append(buf, make([]byte, size)...), nil
}
