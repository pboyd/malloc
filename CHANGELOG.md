# Changelog

## [1.0.0] - 2026-02-14

### Added

- Thread-safe arenas using `sync.RWMutex` for concurrent allocation and deallocation
- Dynamic growth capability through the `ArenaBackend` interface
- `Backend()` option for configuring memory allocation strategy
- `SliceBackend` for arenas that grow using Go slices
- `MmapBackend()` for memory-mapped allocation on Linux, macOS, and Windows
- `FreeableArenaBackend` interface for backends that can free old buffers
- Archived buffer tracking to support growth while preserving existing allocations
- Support for Go 1.25 and 1.26

### Changed

- `NewArena()` signature now accepts optional `opts ...Opt` parameters (backwards compatible)
- `NewArenaAt()` signature now accepts optional `opts ...Opt` parameters (backwards compatible)
- Arenas now grow automatically when out of memory (if backend supports it)
- `Arena.Size()` documentation clarified to exclude archived buffers
- `Arena.Contains()` now checks both active and archived buffers
- `Arena.Free()` now accepts non-word-aligned sizes (rounds up to word boundary)

### Fixed

- `Arena.Contains()` now correctly handles pointers in archived buffers after growth
- Double-free detection improved with better panic messages
- Memory corruption prevention during concurrent growth operations

[1.0.0]: https://github.com/pboyd/malloc/compare/v0.10.0...v1.0.0

## [0.10.0] - 2026-01-31

### Added

- `NewArenaAt()` function to create arenas from existing buffers
- `MallocSlice()` function to allocate slices with arena-backed data
- `FreeSlice()` function to deallocate slices
- Example stack implementation demonstrating arena usage (`example/stack.go`)
- Double-free error detection for common scenarios
- Memory zeroing for newly allocated objects and slices
- Nil pointer detection in `Free()`

### Changed

- `Malloc()` now returns `nil` for zero-byte allocations instead of allocating minimum size

### Fixed

- Backward free block merge operations now work correctly
- Documentation fixes (README typos, capacity calculation corrections)
- Unused variable in tests removed

[0.10.0]: https://github.com/pboyd/malloc/compare/v0.9.0...v0.10.0

## [0.9.0] - 2022-12-21

Initial release

[0.9.0]: https://github.com/pboyd/malloc/releases/tag/v0.9.0
