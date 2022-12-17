package malloc

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestMallocAll(t *testing.T) {
	assert := assert.New(t)

	// Allocate everything in the arena in a single array.
	a := NewArena(64)
	p, err := Malloc[[64]byte](a)
	if !assert.NoError(err) {
		return
	}

	// Set every value in the pointer and ensure the raw memory dump matches
	// exactly.
	for i := 0; i < len(*p); i++ {
		(*p)[i] = byte(i)
	}
	for i, b := range a.Raw() {
		assert.Equal(byte(i), b)
	}

	// Finally verify that no more memory can be allocated.
	overflow, err := Malloc[byte](a)
	assert.Nil(overflow)
	assert.Error(err, ErrOutOfMemory)
}

func TestMallocAllInts(t *testing.T) {
	assert := assert.New(t)

	a := NewArena(64)

	// We can only get 7 words with the current algorithm due to waste.
	for i := 0; i < 7; i++ {
		p, err := Malloc[uint64](a)
		if !assert.NoError(err) {
			return
		}
		*p = ^uint64(0)
	}

	overflow, err := Malloc[byte](a)
	assert.Nil(overflow)
	assert.Error(err, ErrOutOfMemory)
}

func TestMallocStruct(t *testing.T) {
	assert := assert.New(t)

	type Arbitrary struct {
		Field1 int64
		Field2 float64
		Field3 [16]byte
	}

	a := NewArena(128)
	p, err := Malloc[Arbitrary](a)
	if !assert.NoError(err) {
		return
	}

	assert.True(a.Contains(p))
}

func TestMallocFunction(t *testing.T) {
	assert := assert.New(t)
	type IntFunc func(int) int

	a := NewArena(128)
	double, err := Malloc[IntFunc](a)
	if !assert.NoError(err) {
		return
	}

	*double = func(x int) int {
		return x * 2
	}

	assert.True(a.Contains(double))
	assert.Equal(4, (*double)(2))
}
