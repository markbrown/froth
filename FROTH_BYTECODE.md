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
- `'free-vars`: Array of identifiers indexed by context slot (`['x 'y ...]`)
- `'bound-set`: Map of bound identifiers
- `'dead-set`: Map of identifiers whose last use is capture
- `'max-slots`: Frame slots needed
- `'needs-frame`: 0 if frame needed, 1 if not
- `'func-addr`: Bytecode entry point (added by codegen)

**Generator nodes** contain:
- `'body`: Array of child nodes (generators share outer frame, passes recurse into them)

**Other nodes** (literal, identifier, binder, quote, apply) contain type-specific analysis keys as documented in FROTH_COMPILER.md.

A `make-node` function creates the appropriate node type based on the term.

## Compilation Algorithm

Two functions handle compilation:

### compile-func

Compiles a quoted function term to bytecode.

```
compile-func (addr code-map func -- next-addr new-code-map func-node)
```

- `addr`: Next available bytecode address
- `code-map`: Tree23 from function ref IDs to compiled func-nodes
- `func`: Quoted function term to compile

Returns:
- `next-addr`: Next available address after compilation
- `new-code-map`: Updated with this function and all nested functions
- `func-node`: Complete node with `'func-addr`, `'free-vars`, etc.

**Algorithm:**

1. Check `code-map` for existing entry (using `ref` of func). If found, return cached func-node.
2. Run `preflight` on func. If it fails, abort.
3. **Build node tree:** Create func-node with `'term = func`. Scan func's terms left-to-right, building `'body` array:
   - Simple terms (literals, identifiers, binders, quotes, apply): create node with `'term`
   - Nested functions: recursively call `compile-func`, store returned func-node in `'body`
   - Generators: recursively scan (not compile) to build generator node's `'body`
4. **Run passes:** `func-node boundness! liveness! slots!` — each pass traverses `'body`, reads `'term` as needed, and returns updated func-node. Passes recurse into generators but not nested functions (which already have `'free-vars`).
5. **Generate bytecode:** `addr func-node codegen!` — returns `(next-addr func-node)` with `'func-addr` added.
6. **Update code-map:** Store func-node keyed by `ref(func)`.

**Pass signatures:**
- `boundness: (func -- func-node)`
- `liveness: (func-node -- func-node)`
- `slots: (func-node -- func-node)`
- `codegen: (addr code-map func-node -- next-addr new-code-map func-addr)`

**Why passes don't recurse into nested functions:** They read `'free-vars` directly from the nested func-node:
- Boundness: determines which outer vars are captured
- Liveness: determines dead-set (vars only used by capture)
- Slots: determines which vars to push for context building
- Codegen: reads `'func-addr` for close instruction

### compile

Compiles a closureval to a bytecodeval.

```
compile (addr code-map closureval -- next-addr new-code-map bytecodeval)
```

**Algorithm:**

1. `open` closureval to get environment and function body
2. Call `compile-func` on the function body, getting func-node
3. Read `'free-vars` from func-node (array of identifiers)
4. Build context array: for each identifier, get its value from the environment. If the value is a closureval, recursively compile it.
5. Read `'func-addr` from func-node
6. `close` context array and func-addr to create bytecodeval

**Deduplication:** The code-map caches complete func-nodes keyed by function ref. Functions with identical bodies share the same node (and bytecode), even with different captured environments.

## Limitations

Compilation runs `preflight` and can fail for closurevals using dynamic constructs (`env`, `import`, `applyOperator`). Failed compilations leave the environment unchanged.