## Malloc

[![Go Reference](https://pkg.go.dev/badge/github.com/pboyd/malloc.svg)](https://pkg.go.dev/github.com/pboyd/malloc)

This is a memory allocator for Go. The arena holds a chunk of memory and hands
it out as needed, similar to `malloc(3)`. By default, the underlying memory is
taken from Go's heap, but the `MmapBackend` allocates memory through `mmap(2)`
(or `VirtualAlloc` on Windows).

Basic usage:

```go
package main

import (
	"fmt"

	"github.com/pboyd/malloc"
)

func main() {
	arena := malloc.NewArena(4096, malloc.Backend(malloc.MmapBackend(0, 0)))
	intSlice, _ := malloc.MallocSlice[int](arena, 200)
	defer malloc.FreeSlice(arena, intSlice)

	// The data for intSlice is allocated in an mmap'd page.

	fmt.Printf("len=%d, cap=%d\n", len(intSlice), cap(intSlice))
}
```

For a more complete example see [example/stack.go](https://github.com/pboyd/malloc/blob/main/example/stack.go).

This package uses a simple first-fit algorithm to allocate memory. The algorithm has some drawbacks:

- It allocates a minimum of 16 bytes.
- It rounds up sizes not divisible by 16.
- It may fragment easily.

## Credits

This implements Donald Knuth's first-fit memory allocator from The Art of Computer Programming Vol. 1.
