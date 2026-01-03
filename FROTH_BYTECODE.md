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

## Compilation Algorithm

```
compile (addr code-map env-map id -- new-addr new-code-map new-env-map)
```

- `addr`: Next available bytecode address
- `code-map`: Tree23 from function ref IDs to bytecode addresses (deduplication)
- `env-map`: Map from identifiers to values (closurevals → bytecodevals)
- `id`: Quoted identifier to compile

```
0 tree-empty! env 'my-function compile!
/new-env /new-code-map /new-addr
```

**Algorithm:**

1. Look up `id` in `env-map`; skip if not closureval or already bytecodeval
2. `open` closureval to get environment and body; run `preflight`
3. Use `ref` on body to get unique ID; check `code-map` for existing code
4. If not found: run analysis passes, run codegen, update `code-map`
5. Build context array: for each free variable, compile if closureval
6. Create bytecodeval via `close`, update `env-map`

**Dependencies:**

- **Nested functions** (`{ ... }` in body): Compile function body; context built at runtime
- **Captured closurevals** (in environment): Compile body AND build context at compile time

**Deduplication:** `ref` returns unique ID for each function term. The `code-map` (tree23) maps these to bytecode addresses, enabling code sharing across closurevals with same body but different environments.

## Codegen Details

See FROTH_COMPILER.md for the `codegen` function signature and supported terms.

Codegen walks the function body left-to-right, collecting bytecode into an array. When it encounters a nested function, it recursively codegens that function first (emitting at `addr`), then continues collecting. After processing all terms, it emits the collected bytecode. This ensures nested functions appear first in the bytecode store.

**Nested function handling:**

1. Use `ref` on nested function; check `code-map` for existing address
2. If not found: recurse, get `func-addr`, update `code-map`
3. Collect code to build context array from current frame/context slots
4. Collect code to push the nested function's `func-addr`
5. Collect `op close` to create bytecodeval at runtime

The `free-vars` map tells which variables to capture. At runtime, these are looked up from the enclosing function's frame (bound variables) or context (free variables).

## Limitations

Compilation runs `preflight` and can fail for closurevals using dynamic constructs (`env`, `import`, `applyOperator`). Failed compilations leave the environment unchanged.