# Froth Bytecode Compiler Design

## Overview

A bytecode compiler for Froth to improve execution performance by eliminating tree-walking overhead.

## Design Decisions

### Global Bytecode Array

- Single global array of integers holds all compiled bytecode
- Code addresses are integer indices into this array
- Array grows automatically (doubles capacity when full)
- Bytecode is never freed (similar to the string table)

### Threading with Unique Modes

The bytecode array and size are threaded as separate top-level arguments:
```mercury
array(int)::array_di, array(int)::array_uo  % bytecode array
int::in, int::out                            % bytecode size (count of emitted codes)
```

This mirrors the datastack pattern and avoids uniqueness issues with arrays inside data structures.

### Compiled Closure Representation

Compiled closures will have:
- **Context array**: Environment values indexed by slot (not a map)
- **Code address**: Integer index into the global bytecode array

The compiler analyzes free variables and assigns slot indices, replacing O(log n) map lookups with O(1) array access.

### Hybrid Interpreter

- The existing tree-walking interpreter remains
- When `!` encounters a compiled closure, it dispatches to the bytecode VM
- Allows gradual migration and graceful fallback

### Frame Stack

- Located at the top of the datastack array, growing downwards
- Collision detection prevents stack overflow
- Frame contents: just local slots (no saved FP or linkage)
- ENTER n / LEAVE n must use matching sizes
- No frame reflection supported

### Caller/Callee Convention

- **Caller** saves machine registers (return address, context pointer)
- **Callee** creates/destroys activation frames via ENTER/LEAVE
- Simple functions like `{}` or `{1}` need no frame at all
- Identity: just `RETURN`
- Constant: `PUSH_INT 1; RETURN`
- With binder: `ENTER 1; STORE_LOCAL 0; ...; LEAVE 1; RETURN`

### Explicit Compilation

Compilation is user-controlled via a `compile` operator:
```froth
{ x -> x x * } compile!   ; returns bytecode closure or fails
```

Compilation fails for closures using:
- `env` (needs map-based environment access)
- `restore` (dynamically replaces environment)
- Possibly `import` (dynamic loading)

### Bytecode Instructions

All instruction names use camelCase. Frame stack grows downwards.

```
; Frames
enterFrame n       ; frame pointer -= n (expand if necessary)
leaveFrame n       ; frame pointer += n (throw on underflow)

; Register save/restore (caller-saves convention)
saveContextPtr n   ; save context array to frame slot n
restoreContextPtr n; restore context array from frame slot n
saveReturnPtr n    ; save return pointer to frame slot n
restoreReturnPtr n ; restore return pointer from frame slot n

; Values
pushInt n          ; push integer n
pushString n       ; push string with intern ID n

; Variables
pushContext n      ; push value from context slot n
pushLocal n        ; push value from frame slot n (frame_ptr + n)
popLocal n         ; pop value into frame slot n
popUnused          ; pop and discard value

; Operators
op n               ; call operator number n

; Generators
startArray         ; push datastack pointer onto generator stack
endArray           ; pop generator stack, extract array from datastack

; Control
call               ; pop closure, set return ptr, set context, jump
tailCall           ; same as call but omit setting return ptr
return             ; jump to return pointer
```

### Nested Closures

- Nested closures are compiled recursively to bytecode
- If compilation fails for a nested function, the parent compilation also fails
- Caveat: Re-entering the term evaluator may cause problems since it expects to update the environment; this will be addressed later

### Current Implementation

- `bytecode.m`: Module with `init/2` and `emit/5` predicates
- `emit` operator: Appends an integer to the bytecode store from Froth
- `bytecodeval(context, addr)`: New value type for compiled closures
- `close` operator overloaded: `array + int → bytecodeval` (in addition to `map + function → closureval`)

## Resolved Decisions

1. **Stack traces**: Deferred for later consideration.

2. **Tail call detection**: A call is a tail call if `!` is the last term in the function body. This is a simple syntactic check during compilation.

3. **Quoted terms**: Not supported in compiled code. Compilation fails if there are any quoted terms. A constant pool may be added later.
