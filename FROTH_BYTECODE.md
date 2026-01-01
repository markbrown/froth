# Froth Bytecode Compiler Design

## Overview

A bytecode compiler for Froth to improve execution performance by eliminating tree-walking overhead.

## Compiler Pipeline

The compiler consists of four passes that analyze a function and produce metadata for code generation:

1. **Boundness** (left-to-right): Classifies identifiers as bound (local) or free (captured), assigns context slots to free variables
2. **Liveness** (right-to-left): Marks last references, dead binders, and determines register save/restore points
3. **Slots** (left-to-right): Allocates frame slots for bound variables and register saves, with slot reuse
4. **Codegen** (left-to-right): Emits bytecode based on the metadata from previous passes

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

- `enterFrame` is emitted when the first slot is needed (binder or save)
- `leaveFrame` is emitted after the last term using the frame (marked with `'leave-frame=0`)

Functions with no binders and only tail calls need no frame at all.

## Bytecode Instructions

```
; Frames
enterFrame n       ; allocate n frame slots (FP -= n)
leaveFrame n       ; deallocate n frame slots (FP += n)

; Register save/restore (push/pop to stack, then store/load from frame)
saveContextPtr     ; push context array onto stack
restoreContextPtr  ; pop context array from stack
saveReturnPtr      ; push return pointer onto stack
restoreReturnPtr   ; pop return pointer from stack

; Values
pushInt n          ; push integer n
pushString n       ; push string with intern ID n

; Variables
pushContext n      ; push value from context slot n
pushLocal n        ; push value from frame slot n
popLocal n         ; pop value into frame slot n
popUnused          ; pop and discard value

; Operators
op n               ; execute operator number n

; Generators
startArray         ; save SP for array collection
endArray           ; collect values since saved SP into array

; Control
call               ; pop closure, set RP to next instruction, jump
return             ; jump to RP (or exit VM if RP = -1)
```

## Call Sequence

**Non-tail call with saves:**
```
saveContextPtr
popLocal <ctx-slot>
saveReturnPtr
popLocal <rp-slot>
<push closure>
call
pushLocal <rp-slot>
restoreReturnPtr
pushLocal <ctx-slot>
restoreContextPtr
```

**Tail call (no saves needed):**
```
<push closure>
call
```

## Bytecode Store

- Single global array accessed via `peek` and `poke` operators
- `peek addr` returns value at addr (0 if beyond size)
- `poke value addr` writes value, extending array if needed
- Bytecode is never freed (like the string table)

## Compiled Closure Representation

`bytecodeval(context, addr)` where:
- `context`: Array of captured free variable values (indexed by slot)
- `addr`: Starting address in the bytecode store

Created via `close` with array + int arguments.

## Limitations

Compilation fails for closures using:
- `env` (needs map-based environment)
- `restore` (dynamically replaces environment)
- Quoted terms (would need a constant pool)
