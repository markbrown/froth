# Froth Bytecode Compiler Design

## Overview

A bytecode compiler for Froth to improve execution performance by eliminating tree-walking overhead.

## Compiler Pipeline

1. **Preflight**: Checks for unsupported constructs (`env`, `import`, `applyOperator`)
2. **Boundness**: Classifies identifiers as bound or free, assigns context slots
3. **Liveness**: Marks last references, dead binders, register save/restore points
4. **Slots**: Allocates frame slots with reuse
5. **Codegen**: Emits bytecode

See FROTH_COMPILER.md for analysis pass documentation and bytecode helpers.

## Node Structure

Nodes are the single source of truth for a function and its metadata. Each node contains:

- `'term`: The original syntax term (required for all nodes)
- Type-specific keys from analysis passes

**Function nodes** additionally contain:
- `'body`: Array of child nodes (one per term in function body)
- `'free-vars`: Map from identifier to context slot number
- `'free-vars-array`: Array of identifiers indexed by context slot
- `'bound-set`: Map of bound identifiers
- `'dead-set`: Map of identifiers whose last use is capture
- `'max-slots`: Frame slots needed
- `'needs-frame`: 0 if frame needed, 1 if not
- `'func-addr`: Bytecode entry point (added by codegen)

**Generator nodes** contain:
- `'body`: Array of child nodes (generators share outer frame, passes recurse into them)

**Other nodes** (literal, identifier, binder, quote, apply) contain type-specific analysis keys as documented in FROTH_COMPILER.md.

A `make-node` function creates the appropriate node type based on the term.

## Current Implementation

The compiler pipeline chains the individual passes:

```
func boundness! liveness! slots! /func-node
0 tree-empty! func-node codegen! /func-addr /code-map /next-addr
```

**Pass signatures:**
- `boundness: (func -- func-node)` — builds node tree, classifies variables
- `liveness: (func-node -- func-node)` — marks last references, tail calls
- `slots: (func-node -- func-node)` — allocates frame slots
- `codegen: (addr code-map func-node -- next-addr new-code-map func-addr)` — emits bytecode

**Key behaviors:**
- Passes recurse into generators (which share the outer frame)
- Passes do NOT recurse into nested functions—they read `'free-vars` from the already-analyzed nested func-node
- Codegen handles nested functions by recursively compiling them and building context arrays

## Planned: Higher-Level Compilation API

The following functions are planned but not yet implemented:

### compile-func (planned)

```
compile-func (addr code-map func -- next-addr new-code-map func-node)
```

Would combine preflight checking, pass chaining, and caching into a single function that:
1. Checks `code-map` for existing entry (deduplication)
2. Runs `preflight` to check for dynamic constructs
3. Chains the passes: `func boundness! liveness! slots!`
4. Runs codegen: `addr code-map func-node codegen!`
5. Caches the result in `code-map`

### compile (planned)

```
compile (addr code-map closureval -- next-addr new-code-map bytecodeval)
```

Would compile a closureval to a bytecodeval by:
1. Opening the closureval to get environment and function body
2. Calling `compile-func` on the function body
3. Building a context array from the environment using `'free-vars`
4. Creating a bytecodeval with `close`

## Limitations

Compilation runs `preflight` and can fail for closurevals using dynamic constructs (`env`, `import`, `applyOperator`). Failed compilations leave the environment unchanged.