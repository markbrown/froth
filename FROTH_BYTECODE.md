# Froth! Bytecode Compiler Design

## Overview

A bytecode compiler for Froth! to improve execution performance by eliminating tree-walking overhead.

See FROTH_COMPILER.md for detailed pass documentation, node structure, and API reference.

## Key Behaviors

**Bottom-up compilation**: When `compile-func` encounters a nested function term, it recursively compiles the nested function first. The resulting func-node (with `'func-addr` already set) replaces the original term in the outer function's body.

**Passes don't recurse into nested functions**: The analysis passes (boundness, liveness, slots) and codegen read data from already-complete nested func-nodes rather than recursing into them. They do recurse into generators, which share the outer frame.

**Codegen reads `'func-addr` from nested nodes**: For nested function terms, codegen emits code to build the context array and create a closure using the pre-computed `node 'func-addr @`. It does not recursively compile.

## Limitations

Compilation can fail for functions using dynamic constructs (`env`, `import`, `applyOperator`) that prevent static analysis.
