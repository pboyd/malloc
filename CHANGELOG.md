# Changelog

## [1.2.1] - 2026-02-24

### Added

- `VirtReservationBackend.Addr()` method to retrieve the base address of the reserved virtual memory region.
- `VirtReservationBackend.Release()` method to free the entire reservation when the address is not acceptable or the backend is no longer needed.

### Changed

- `VirtBackend()` now returns `*VirtReservationBackend` instead of `ArenaBackend`, exposing `Addr` and `Release` on the concrete type. **Breaking change.**

[1.2.1]: https://github.com/pboyd/malloc/compare/v1.2.0...v1.2.1

## [1.2.0] - 2026-02-20

### Added

- `VirtBackend` for fixed-capacity virtual memory arenas: reserves a contiguous virtual address range at creation and commits pages on demand. Each `Grow` call extends the committed region in place, keeping the base address constant. When the arena reaches capacity, `Grow` returns `ErrOutOfMemory`. Implements `ProtectedArenaBackend`.

### Changed

- `MmapBackend` now accepts variadic `BackendOpt` options instead of positional `prot` and `flags` integers. Use `MmapProt`, `MmapFlags`, and `MmapAddr` to configure the mapping; `MmapAddr` requests a specific mapping address. **Breaking change.**

### Fixed

- Exec protection flag for `MmapBackend` on Windows.

[1.2.0]: https://github.com/pboyd/malloc/compare/v1.1.0...v1.2.0

## [1.1.0] - 2026-02-15

### Added

- `ProtectedArenaBackend` interface for changing memory protections on allocated pages
- `MmapBackend.Protect()` method to set page permissions on memory-mapped allocations
- Cross-compilation verification for FreeBSD, OpenBSD, and NetBSD

### Fixed

- Build compatibility with BSD variants (FreeBSD, OpenBSD, NetBSD)

[1.1.0]: https://github.com/pboyd/malloc/compare/v1.0.0...v1.1.0

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
