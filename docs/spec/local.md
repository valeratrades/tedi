# Local Storage Specification

This document defines the rules for local issue storage operations.

## Filesystem Mutation

r[local.sink-only-mutation]

Only `Sink<Submitted>` and `Sink<Consensus>` implementations may mutate the local filesystem.
All filesystem writes, deletes, and directory creation MUST go through these implementations.

This ensures:
- Single point of control for all mutations
- Consistent handling of file format conversions (flat file <-> directory)
- Proper git staging for consensus state

## LocalPath Reader Abstraction

r[local.path-reader-only]

All filesystem interactions in `LocalPath` MUST go through `LocalReader` trait methods.
Never use `std::fs`, `Path::exists()`, `Path::is_dir()`, etc. directly.
And especially, never construct paths manually! Even if operation seems trivial.

Allowed: `reader.exists()`, `reader.is_dir()`, `reader.list_dir()`, `reader.read_content()`
Forbidden: `std::fs::*`, `path.exists()`, `path.is_dir()`, `path.is_file()`

This ensures the same `LocalPath` code works for both:
- `FsReader`: reads current filesystem state (submitted)
- `GitReader`: reads git HEAD state (consensus)
