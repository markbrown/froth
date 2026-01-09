# Froth! Bytecode Compiler Design

## Overview

A bytecode compiler for Froth! to improve execution performance by eliminating tree-walking overhead.

See FROTH_COMPILER.md for detailed pass documentation, node structure, and API reference.

## Key Behaviors

**Bottom-up compilation**: When `compile-func` encounters a nested function term, it recursively compiles the nested function first. The resulting func-node (with `'func-addr` already set) replaces the original term in the outer function's body.

**Passes don't recurse into nested functions**: The analysis passes (boundness, liveness, slots) and codegen read data from already-complete nested func-nodes rather than recursing into them. They do recurse into generators, which share the outer frame.

**Codegen reads `'func-addr` from nested nodes**: For nested function terms, codegen emits code to build the context array and create a closure using the pre-computed `node 'func-addr @`. It does not recursively compile.

## Liveness Pass

Liveness reads node fields to mark last references and tail calls. All nodes have `'term` from node creation.

| Field | Node Types | Source | Description |
|-------|------------|--------|-------------|
| `'bound-set` | generator | boundness | Set of bound identifiers |
| `'free-vars-map` | function (nested) | boundness | Captured vars with context slots |
| `'is-bound` | identifier | boundness | 0 = bound, 1 = free |
| `'is-operator` | identifier | boundness | 0 = operator, 1 = variable |
| `'parent-free-vars-map` | function (nested) | boundness | Outer's free-vars-map |
| `'body` | function, generator | node creation | Array of child nodes |

## Slots Pass

Slots reads node fields to allocate frame slots. All nodes have `'term` from node creation.

| Field | Node Types | Source | Description |
|-------|------------|--------|-------------|
| `'is-bound` | identifier | boundness | 0 = bound, 1 = free |
| `'is-operator` | identifier | boundness | 0 = operator, 1 = variable |
| `'dead-set` | function (nested) | liveness | Vars whose last use is capture |
| `'is-live` | identifier | liveness | 0 = still live, 1 = last reference |
| `'is-tail-call` | apply | liveness | 0 = tail call |
| `'is-used` | binder | liveness | 0 = used, 1 = dead |
| `'needs-ctx-save` | function | liveness | 0 = needs context save before first call |
| `'restore-context` | apply | liveness | 0 = needs context restore |
| `'restore-return` | apply | liveness | 0 = last non-tail call |
| `'body` | function, generator | node creation | Array of child nodes |

## Codegen Pass

Codegen reads node fields computed by earlier passes to emit bytecode. All nodes have `'term` from node creation.

| Field | Node Types | Source | Description |
|-------|------------|--------|-------------|
| `'free-vars-array` | function (nested) | boundness | Captured vars in slot order |
| `'is-bound` | identifier | boundness | 0 = bound, 1 = free |
| `'is-operator` | identifier | boundness | 0 = operator, 1 = variable |
| `'parent-free-vars-map` | function (nested) | boundness | Outer's free-vars-map |
| `'slot` | binder, identifier | boundness or slots | Context slot (free) or frame slot (bound) |
| `'func-addr` | function (nested) | codegen | Bytecode entry point |
| `'is-tail-call` | apply | liveness | 0 = tail call |
| `'is-used` | binder | liveness | 0 = used, 1 = dead |
| `'leave-frame` | identifier, function, apply | liveness | 0 = last frame access |
| `'needs-frame` | function | liveness | 0 = frame needed |
| `'restore-context` | apply | liveness | 0 = needs context restore |
| `'restore-return` | apply | liveness | 0 = last non-tail call |
| `'body` | function, generator | node creation | Array of child nodes |
| `'ctx-save-slot` | apply | slots | Slot for context pointer |
| `'max-slots` | function | slots | Frame slots needed |
| `'parent-bound-vars-map` | function (nested) | slots | Outer's slot-map |
| `'rp-save-slot` | apply | slots | Slot for return pointer |
| `'save-context` | apply | slots | 0 = first call needing context save |
| `'save-return` | apply | slots | 0 = first non-tail call |

## Limitations

Compilation can fail for functions using dynamic constructs (`env`, `import`, `applyOperator`) that prevent static analysis.
