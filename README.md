## Malloc

[![Go Reference](https://pkg.go.dev/badge/github.com/pboyd/malloc.svg)](https://pkg.go.dev/github.com/pboyd/malloc)

This is a memory allocator for Go. The arena holds a chunk of memory and hands it out as needed, similar to `malloc(3)`.

Basic usage:

```go
arena := malloc.NewArena(1024)
pointer := malloc.Malloc[SomeStruct](arena)
defer malloc.Free(arena, pointer)

// pointer now points to a SomeStruct in the arena.
```

For a more complete example see [example/stack.go](https://github.com/pboyd/malloc/blob/master/example/stack.go).

This package uses a simple first-fit algorithm to allocate memory. The algorithm has some drawbacks:

- It allocates a minimum of 16 bytes.
- It rounds up sizes not divisible by 16.
- It may fragment easily.

## Credits

This implements Donald Knuth's first-fit memory allocator from The Art of Computer Programming Vol. 1.
