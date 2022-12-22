## Malloc

[![Go Reference](https://pkg.go.dev/badge/github.com/pboyd/malloc.svg)](https://pkg.go.dev/github.com/pboyd/malloc)

This is a proof-of-concept memory allocator for Go. A fixed-size chunk of
memory is held by the arena and handed out as needed, similar to `malloc(3)`.

Basic usage:

```go
arena := malloc.NewArena(1024)
pointer := malloc.Malloc[SomeStruct](a)
defer malloc.Free(pointer)

// pointer is now a *SomeStruct allocated inside the arena.
```

For a more complete example see [example/stack.go](https://github.com/pboyd/malloc/example/stack.go).

It uses a fairly primitive first-fit algorithm to allocate memory. This is
simple to implement for a proof of concept, but it has some drawbacks:

- The smallest size that can be allocated is 16 bytes.
- Sizes which aren't divisible by 16 will be rounded up.
- It probably fragments easily.

## Credits

The algorithm is taken from Donald Knuth's first-fit memory allocator in The
Art of Computer Programming Vol. 1.
