package example

import (
	"errors"
	"unsafe"

	"github.com/pboyd/malloc"
)

// ErrStackOverflow is returned by Push when the memory has been exhausted.
var ErrStackOverflow = errors.New("stack overflow")

// ErrStackUnderflow is returned by Pop when the stack is empty.
var ErrStackUnderflow = errors.New("stack underflow")

// Stack is a simple stack for a single data type which uses a fixed amount of memory.
type Stack[T any] struct {
	arena *malloc.Arena
	top   *stackItemHeader
}

type stackItemHeader struct {
	data unsafe.Pointer
	prev *stackItemHeader
}

// NewStack returns a stack using a specific amount of memory. Size is in
// bytes. The actual memory size may be rounded up.
func NewStack[T any](size uint64) *Stack[T] {
	return &Stack[T]{
		arena: malloc.NewArena(size),
	}
}

// Push adds a copy of an item to the stack.
//
// Returns ErrStackOverflow if the stack is full.
func (s *Stack[T]) Push(item *T) error {
	header, err := s.newItemHeader()
	if err != nil {
		return err
	}

	data, err := malloc.Malloc[T](s.arena)
	if err != nil {
		malloc.Free(s.arena, header)
		if errors.Is(err, malloc.ErrOutOfMemory) {
			return ErrStackOverflow
		}
		return err
	}

	*data = *item
	header.data = unsafe.Pointer(data)

	header.prev = s.top
	s.top = header

	return nil
}

func (s *Stack[T]) newItemHeader() (*stackItemHeader, error) {
	header, err := malloc.Malloc[stackItemHeader](s.arena)
	if err != nil {
		if errors.Is(err, malloc.ErrOutOfMemory) {
			return nil, ErrStackOverflow
		}
		return nil, err
	}
	return header, nil
}

// Pop removes the last item pushed onto the stack and returns it.
//
// If the stack is empty ErrStackUnderflow is returned.
func (s *Stack[T]) Pop() (*T, error) {
	if s.top == nil {
		return nil, ErrStackUnderflow
	}

	item := (*T)(s.top.data)
	dup := *item

	oldTop := s.top
	s.top = s.top.prev

	malloc.Free(s.arena, item)
	malloc.Free(s.arena, oldTop)

	return &dup, nil
}
