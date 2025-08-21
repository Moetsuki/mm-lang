# C Backend Module

[← Back to README](../README.md)

## Overview

The C backend (`src/backend_c/target_c.rs`) lowers the AST into portable C code, which is then compiled by Clang to produce a native executable. This backend mirrors the semantics of the LLVM backend and is exercised by the same test suite.

Type mapping is centralized in `src/backend_c/type_c.rs`.

## Design

- Statement and expression transformers generate C statements/expressions as strings.
- Declarations, function signatures, and struct/class shapes are emitted before use.
- The backend collects header‑style declarations (`decls`) and definition bodies, then concatenates them into the final C translation unit.

## Type Mapping

Types map to C as follows (selected):
- `bool` → `bool`
- Signed integers → `int8_t`, `int16_t`, `int32_t`, `int64_t`
- Unsigned integers → `uint8_t`, `uint16_t`, `uint32_t`, `uint64_t`
- `f32` → `float`, `f64` → `double`
- `string` → `const char*`
- Pointers → `T*`
- `tensor[T]` → lowered using element type `T`
- `class Name` / `struct Name` → `struct Name`

See `type_c.rs` for the exact mapping logic.

## Features Covered

- Variables and assignments
- Arithmetic, comparisons, logical ops
- Functions, returns, calls
- Blocks and if/else
- Strings and `printf`
- Tensors (initializer lists and indexing)
- Structs (declarations, literals, field access/assignment)
- Classes (fields, methods, constructors) with basic vtable layout mirrored in C

## Notes

- Visibility is not enforced yet (parsed and preserved for layout only).
- Logical operators are currently eager.
- The backend is intended to produce simple, readable C for debugging and portability.
