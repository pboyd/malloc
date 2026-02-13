package malloc

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestSliceExpander(t *testing.T) {
	assert := assert.New(t)

	a := NewArena(64, Expander(SliceExpander))
	p1, err := Malloc[[64 - wordSize]byte](a)
	if !assert.NoError(err) {
		return
	}

	p2, err := Malloc[[64]byte](a)
	if !assert.NoError(err) {
		return
	}

	Free(a, p1)
	assert.Len(a.archive, 0, "archive should be cleared when the last pointer is freed")

	Free(a, p2)
	assert.Equal(128-wordSize, a.Cap())
}

type growingSliceExpander struct {
	s []byte
}

func (e *growingSliceExpander) Grow(buf []byte, size uintptr) ([]byte, error) {
	newSize := len(buf) + int(size)
	if newSize > cap(e.s) {
		return nil, ErrOutOfMemory
	}
	return e.s[:newSize], nil
}

func TestGrowingExpander(t *testing.T) {
	t.Run("Grow from full", func(t *testing.T) {
		assert := assert.New(t)

		a := NewArena(64, Expander(&growingSliceExpander{s: make([]byte, 128)}))
		p1, err := Malloc[[64 - wordSize]byte](a)
		if !assert.NoError(err) {
			return
		}

		p2, err := Malloc[[64]byte](a)
		if !assert.NoError(err) {
			return
		}

		assert.Len(a.archive, 0, "archive should be unused")

		Free(a, p1)
		Free(a, p2)
		assert.Equal(128-wordSize, a.Cap())
		assert.Equal(128-wordSize, a.FreeBytes())
	})

	t.Run("Grow from empty", func(t *testing.T) {
		assert := assert.New(t)

		a := NewArena(48, Expander(&growingSliceExpander{s: make([]byte, 128)}))
		p1, err := Malloc[[80]byte](a)
		if !assert.NoError(err) {
			return
		}

		assert.Len(a.archive, 0, "archive should be unused")

		Free(a, p1)
		assert.Equal(128-wordSize, a.Cap())
		assert.Equal(128-wordSize, a.FreeBytes())
	})

	t.Run("Grow from fragmented", func(t *testing.T) {
		assert := assert.New(t)

		a := NewArena(64, Expander(&growingSliceExpander{s: make([]byte, 128)}))
		p3, _ := a.Malloc(16)
		p2, _ := a.Malloc(16)
		p1, _ := a.Malloc(16)

		// Free the middle creating a hole
		a.Free(p2, 16)

		p4, err := a.Malloc(64)
		if !assert.NoError(err) {
			return
		}

		assert.Len(a.archive, 0, "archive should be unused")

		a.Free(p1, 16)
		a.Free(p3, 16)
		a.Free(p4, 64)
		assert.Equal(128-wordSize, a.Cap())
		assert.Equal(128-wordSize, a.FreeBytes())
	})
}
