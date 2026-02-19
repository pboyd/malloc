//go:build linux || darwin

package malloc_test

import (
	"fmt"

	"github.com/pboyd/malloc"
)

func ExampleMmapBackend() {
	arena := malloc.NewArena(4096, malloc.Backend(malloc.MmapBackend()))
	intSlice, _ := malloc.MallocSlice[int](arena, 200)
	defer malloc.FreeSlice(arena, intSlice)

	// The underlying data for intSlice is allocated in an mmap'd page.

	fmt.Printf("len=%d, cap=%d\n", len(intSlice), cap(intSlice))
	// Output:
	// len=200, cap=200
}
