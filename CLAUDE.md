# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is SSALang3, a compiler project written in Rust using a workspace structure. The compiler is organized into several distinct crates that implement different compilation phases:

- **lexer**: Tokenization of source code into tokens (TokenType enum with keywords, operators, literals)
- **ast**: Abstract Syntax Tree representation with type-erased and typed node handles
- **ir**: Intermediate Representation with SSA form instructions and basic blocks
- **common**: Shared types and symbol management with interned types and symbols
- **main**: Entry point that orchestrates the compilation pipeline

## Architecture

### AST System
The AST uses a sophisticated arena-based allocation system with both type-erased (`NodeHandle`) and strongly-typed (`TypedNodeHandle<T>`) handles. Node types are encoded in the handle using niche optimization where NodeType 0 is reserved for `Option<Handle>` optimization.

### Type System
Types are interned in a global `TypeStore` with deduplication. The type system supports primitives (bool, i32, unit), structs with sorted fields, and uses `TypeId` for efficient comparisons.

### IR Generation
The IR uses SSA form with basic blocks, phi nodes, and reference types. Instructions are managed through `InstrRef`, `BlockRef`, and `VarRef` handles.

## Common Development Commands

Since this is a Rust workspace project, use standard Cargo commands:

```bash
# Build the entire workspace
cargo build

# Build in release mode
cargo build --release

# Run the main binary
cargo run -p main

# Run tests for all crates
cargo test

# Run tests for a specific crate
cargo test -p lexer
cargo test -p ast
cargo test -p ir

# Check code without building
cargo check

# Format code
cargo fmt

# Run clippy lints
cargo clippy
```

## Crate Dependencies

The dependency graph flows: main → ast → (lexer, common, ir), where common provides shared types and symbols used across multiple crates.