# Froth Bytecode Compiler Design

## Overview

A bytecode compiler for Froth to improve execution performance by eliminating tree-walking overhead.

## Compiler Pipeline

The `compile-func` function orchestrates bottom-up compilation:

1. Build node tree (recurses into nested functions first)
2. **Boundness**: Classifies identifiers as bound or free, assigns context slots
3. **Liveness**: Marks last references, dead binders, register save/restore points
4. **Slots**: Allocates frame slots with reuse
5. **Codegen**: Emits bytecode, stores `'func-addr` in node

Nested functions are compiled before their containing function. This means each pass operates on a function where nested func-nodes are already complete.

See FROTH_COMPILER.md for detailed pass documentation.

## Node Structure

Nodes are the single source of truth for a function and its metadata. Each node contains:

- `'term`: The original syntax term (required for all nodes)
- Type-specific keys from analysis passes

**Function nodes** additionally contain:
- `'body`: Array of child nodes (one per term in function body)
- `'free-vars-map`: Map from identifier to context slot number
- `'free-vars-array`: Array of identifiers indexed by context slot
- `'bound-set`: Map of bound identifiers
- `'dead-set`: Map of identifiers whose last use is capture
- `'max-slots`: Frame slots needed
- `'needs-frame`: 0 if frame needed, 1 if not
- `'func-addr`: Bytecode entry point (added by codegen)

**Generator nodes** contain:
- `'body`: Array of child nodes (generators share outer frame and context)
- `'bound-set`: Map of identifiers bound inside the generator

**Other nodes** (literal, identifier, binder, quote, apply) contain type-specific analysis keys as documented in FROTH_COMPILER.md.

## Pass Signatures

- `compile-func: (code-map addr func -- code-map addr func-node)` — orchestrates compilation
- `boundness: (func-node -- func-node)` — classifies variables
- `liveness: (func-node -- func-node)` — marks last references, tail calls
- `slots: (func-node -- func-node)` — allocates frame slots
- `codegen: (addr func-node -- next-addr func-node)` — emits bytecode, stores `'func-addr` in node

## Key Behaviors

**Bottom-up compilation**: When `compile-func` encounters a nested function term, it recursively compiles the nested function first. The resulting func-node (with `'func-addr` already set) replaces the original term in the outer function's body.

**Passes don't recurse into nested functions**: The analysis passes (boundness, liveness, slots) and codegen read data from already-complete nested func-nodes rather than recursing into them. They do recurse into generators, which share the outer frame.

**Codegen reads `'func-addr` from nested nodes**: For nested function terms, codegen emits code to build the context array and create a closure using the pre-computed `node 'func-addr @`. It does not recursively compile.

**`'func-addr` stored in node**: After emitting bytecode, codegen updates the func-node with `addr 'func-addr :`. This allows outer functions to reference the nested function's bytecode address.

## Limitations

Compilation can fail for functions using dynamic constructs (`env`, `import`, `applyOperator`) that prevent static analysis.