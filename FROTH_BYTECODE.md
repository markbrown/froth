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
compile (addr body-map env-map id -- new-addr new-body-map new-env-map)
```

- `addr`: Next available bytecode address
- `body-map`: Tree23 from body ref IDs to bytecode addresses (deduplication)
- `env-map`: Map from identifiers to values (closurevals → bytecodevals)
- `id`: Quoted identifier to compile

```
0 tree-empty! env 'my-function compile!
/new-env /new-body-map /new-addr
```

**Algorithm:**

1. Look up `id` in `env-map`; skip if not closureval or already bytecodeval
2. `open` closure to get environment and body; run `preflight`
3. Use `ref` on body to get unique ID; check `body-map` for existing code
4. If not found: run analysis passes, emit bytecode, update `body-map`
5. Build context array: for each free variable, compile if closureval
6. Create bytecodeval via `close`, update `env-map`

**Dependencies:**

- **Nested functions** (`{ ... }` in body): Compile body, context built at runtime
- **Captured closures** (in environment): Compile body AND build context at compile time

**Deduplication:** `ref` returns unique ID for each body term. Tree23 maps these to bytecode addresses, enabling code sharing across closures with same body but different environments.

## Limitations

Compilation runs `preflight` and can fail for closures using dynamic constructs (`env`, `import`, `applyOperator`). Failed compilations leave the environment unchanged.
