package example

import (
	"errors"
	"testing"

	"github.com/stretchr/testify/assert"
)

type stackTestItem struct {
	Int   int
	Float float64
}

func TestStack(t *testing.T) {
	assert := assert.New(t)

	stack := NewStack[stackTestItem](1024)

	max := 0
	for i := 0; ; i++ {
		err := stack.Push(&stackTestItem{
			Int:   i,
			Float: float64(i),
		})
		if errors.Is(err, ErrStackOverflow) {
			break
		}
		if assert.NoError(err) {
			max = i
		}
	}

	for i := max; i >= 0; i-- {
		item, err := stack.Pop()
		if assert.NoError(err) {
			assert.Equal(i, item.Int)
			assert.Equal(float64(i), item.Float)
		}
	}

	_, err := stack.Pop()
	assert.ErrorIs(err, ErrStackUnderflow)
}
