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

## Non-Tail Call Handling

Non-tail calls require saving and restoring the context pointer (CP) and return pointer (RP) across the call. The analysis passes provide flags on apply nodes:

**From liveness:**
- `is-tail-call`: 0 = tail call, 1 = non-tail call
- `restore-context`: 0 = needs CP restore after call
- `restore-return`: 0 = needs RP restore after call (last non-tail call)
- `leave-frame`: 0 = last term using the frame

**From slots:**
- `save-context`: 0 = first call needing CP save
- `save-return`: 0 = first non-tail call needing RP save
- `ctx-save-slot`: frame slot for saving CP
- `rp-save-slot`: frame slot for saving RP

**Codegen algorithm for non-tail calls:**

Before call:
1. If `need-enter` = 0 AND (`save-context` = 0 OR `save-return` = 0): emit `enter-frame`, set `need-enter` = 1
2. If `save-context` = 0: emit `save-context-ptr`, `pop-local ctx-save-slot`
3. If `save-return` = 0: emit `save-return-ptr`, `pop-local rp-save-slot`
4. Emit `call`

After call:
5. If `restore-context` = 0: emit `push-local ctx-save-slot`, `restore-context-ptr`
6. If `restore-return` = 0: emit `push-local rp-save-slot`, `restore-return-ptr`
7. If `leave-frame` = 0: emit `leave-frame max-slots`

The save instructions push CP/RP to the data stack; `pop-local` stores them in the frame. Restoring reverses this: `push-local` retrieves from frame, restore instruction sets CP/RP.

## Limitations

Compilation runs `preflight` and can fail for closurevals using dynamic constructs (`env`, `import`, `applyOperator`). Failed compilations leave the environment unchanged.