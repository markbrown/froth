# Froth Bytecode Compiler Design

## Overview

A bytecode compiler for Froth to improve execution performance by eliminating tree-walking overhead.

## Compiler Pipeline

The compiler consists of analysis passes that produce metadata for code generation:

1. **Preflight**: Checks for unsupported constructs (`env`, `import`, `applyOperator`)
2. **Boundness** (left-to-right): Classifies identifiers as bound (local) or free (captured), assigns context slots to free variables
3. **Liveness** (right-to-left): Marks last references, dead binders, and determines register save/restore points
4. **Slots** (left-to-right): Allocates frame slots for bound variables and register saves, with slot reuse
5. **Codegen** (left-to-right): Emits bytecode based on the metadata from previous passes

See FROTH_COMPILER.md for documentation of the analysis passes (`preflight`, `boundness`, `liveness`, `slots`) and bytecode helpers (`instruction-table`, `operator-table`, `emit-at`, `emit-all-at`).

## Frame Slot Allocation

Slots are allocated on-demand as terms are processed:

- **Binders**: Allocate a slot when encountered (if used)
- **Register saves**: Allocate slots at non-tail calls that need saves
- **Slot reuse**: When a variable's last reference is processed, its slot is freed for reuse

Example: `{/x x x * f! 1 +}`
- `/x` allocates slot 0
- Second `x` frees slot 0 (last reference)
- `!` reuses slot 0 for saving RP
- `max-slots` = 1 (peak usage)

## Register Save/Restore

Non-tail calls may need to save and restore registers:

- **Context pointer**: Saved before and restored after each call that needs it
- **Return pointer**: Saved before the leftmost non-tail call, restored after the rightmost

For RP, the save and restore may happen at different calls:
- `f! g! +` — save RP before `f!`, restore after `g!`
- `f! +` — save and restore both happen around `f!`

The liveness pass marks:
- `'restore-context=0` on calls needing context restore
- `'restore-return=0` on the rightmost non-tail call (where RP is restored)

The slots pass allocates:
- `'ctx-save-slot` on calls with `'restore-context=0`
- `'rp-save-slot` on calls with `'restore-return=0`

Codegen saves RP before the first non-tail call it encounters (if the function has any), using the slot from the call with `'restore-return=0`.

## Frame Entry/Exit

- `enter-frame` is emitted when the first slot is needed (binder or save)
- `leave-frame` is emitted after the last term using the frame (marked with `'leave-frame=0`)

Functions with no binders and only tail calls need no frame at all.

## Bytecode Instructions

Instructions are identified by codes from `instruction-table`. See FROTH_COMPILER.md for the full table.

```
; Frames
enter-frame n      ; allocate n frame slots (FP -= n)
leave-frame n      ; deallocate n frame slots (FP += n)

; Register save/restore (push/pop to stack, then store/load from frame)
save-context-ptr   ; push context array onto stack
restore-context-ptr ; pop context array from stack
save-return-ptr    ; push return pointer onto stack
restore-return-ptr ; pop return pointer from stack

; Values
push-int n         ; push integer n
push-string n      ; push string with intern ID n

; Variables
push-context n     ; push value from context slot n
push-local n       ; push value from frame slot n
pop-local n        ; pop value into frame slot n
pop-unused         ; pop and discard value

; Operators
op n               ; execute operator number n (from operator-table)

; Generators
start-array        ; save SP for array collection
end-array          ; collect values since saved SP into array

; Control
call               ; pop closure, set RP to next instruction, jump
tail-call          ; pop closure, preserve RP, jump
return             ; jump to RP (or exit VM if RP = -1)
```

## Call Sequence

**Non-tail call with saves:**
```
save-context-ptr
pop-local <ctx-slot>
save-return-ptr
pop-local <rp-slot>
<push closure>
call
push-local <rp-slot>
restore-return-ptr
push-local <ctx-slot>
restore-context-ptr
```

**Tail call:**
```
<push closure>
tail-call
```

## Bytecode Store

- Single global array accessed via `peek` and `poke` operators
- `peek addr` returns value at addr (0 if beyond size)
- `poke value addr` writes value, extending array if needed
- Bytecode is never freed (like the string table)

## Constant Pool

Separate from the bytecode store, the constant pool stores complex values that can't be inlined as immediate operands.

- `ref value` stores a value and returns its index (with deduplication)
- `deref index` retrieves a value by index
- Pool persists across bytecode calls within a session
- Values are deduplicated: storing the same value twice returns the same index

Use cases:
- String literals (store intern ID, but complex strings may need pool)
- Array literals
- Map literals
- Nested closures captured as constants

```
42 ref /idx           ; store 42, get index (e.g., 0)
42 ref                ; returns same index (deduplicated)
idx deref             ; retrieves 42

[1 2 3] ref /arr-idx  ; store array
arr-idx deref         ; retrieves [1 2 3]
```

For codegen, the compiler can use `ref` at compile time to store constants, then emit `push-int <index>` followed by `op deref` to load them at runtime.

## Compiled Closure Representation

`bytecodeval(context, addr)` where:
- `context`: Array of captured free variable values (indexed by slot)
- `addr`: Starting address in the bytecode store

Created via `close` with array + int arguments. Decomposed via `open` back to array + int.

## Limitations

Compilation fails (preflight returns 1) for closures using:
- `env` (requires dynamic environment access)
- `import` (modifies environment at runtime)
- `applyOperator` (dynamic operator dispatch)

These constructs require the tree-walking interpreter.
