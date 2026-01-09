# Froth! Bytecode Compiler Design

## Overview

A bytecode compiler for Froth! to improve execution performance by eliminating tree-walking overhead.

See FROTH_COMPILER.md for detailed pass documentation, node structure, and API reference.

## Key Behaviors

**Bottom-up compilation**: When `compile-func` encounters a nested function term, it recursively compiles the nested function first. The resulting func-node (with `'func-addr` already set) replaces the original term in the outer function's body.

**Passes don't recurse into nested functions**: The analysis passes (boundness, liveness, slots) and codegen read data from already-complete nested func-nodes rather than recursing into them. They do recurse into generators, which share the outer frame.

**Codegen reads `'func-addr` from nested nodes**: For nested function terms, codegen emits code to build the context array and create a closure using the pre-computed `node 'func-addr @`. It does not recursively compile.

## Known Bug: Context Slot Mismatch for Nested Closures

When a nested closure captures free variables, codegen must build a context array containing the captured values. Currently, it uses the slot numbers from the inner closure's `free-vars-map` directly, but these are the *inner* closure's context slots, not the *outer* scope's slots.

**Example:** `flatten` is defined as:
```froth
['reduce 'concat] { [] {concat!} reduce! } def-fn! /flatten
```

The outer function's context is:
- slot 0 = reduce
- slot 1 = concat

The inner closure `{concat!}` has:
- free-vars-map: `{concat: 0}` (concat is at slot 0 in *its* context)

**Bug:** When building the inner closure's context array, codegen emits `push-context 0` (the inner slot), but it should emit `push-context 1` (where concat actually lives in the outer context).

**Result:** The inner closure receives `reduce` instead of `concat`, causing type errors at runtime.

**Fix needed:** When emitting code to build a nested closure's context, look up each captured variable in the outer scope's `free-vars-map` or `slot-map` (via `outer-free-vars-map` or `outer-bound-vars-map`) to get the correct slot number.

## Limitations

Compilation can fail for functions using dynamic constructs (`env`, `import`, `applyOperator`) that prevent static analysis.
