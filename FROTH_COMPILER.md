# Froth Compiler Reference

This document describes the compiler infrastructure for Froth, including bytecode generation and static analysis passes.

## Quick Reference

| Name | Module | Description |
|------|--------|-------------|
| `boundness` | boundness | Analyze variable binding (returns map) |
| `emit-all-at` | bytecode | Write array to bytecode store |
| `emit-at` | bytecode | Write value to bytecode store |
| `liveness` | liveness | Analyze last references in function |
| `operator-table` | ops | Constant: map from operator identifiers to codes |
| `instruction-table` | bytecode | Constant: map from symbols to VM instruction codes |
| `new-apply-node` | node | Create apply node with defaults |
| `new-binder-node` | node | Create binder node with defaults |
| `new-closure-node` | node | Create closure node with defaults |
| `new-generator-node` | node | Create generator node with defaults |
| `new-identifier-node` | node | Create identifier node with defaults |
| `new-literal-node` | node | Create literal node with defaults |
| `new-node` | node | Create empty base node |
| `new-quote-node` | node | Create quote node with defaults |
| `preflight` | preflight | Check for env/import/applyOperator usage |
| `slots` | slots | Allocate frame slots for function |

## Bytecode (bytecode.froth)

Helpers for bytecode generation. The bytecode store is accessed via the `peek` and `poke` operators.

| Name | Type | Description |
|------|------|-------------|
| `instruction-table` | map | Map from symbols to VM instruction codes |
| `emit-at` | function | `( value addr -- addr' )` Write value at addr, return next addr |
| `emit-all-at` | function | `( arr addr -- addr' )` Write array starting at addr, return next addr |

The `instruction-table` constant maps symbols to VM instruction codes. Available instructions:

| Symbol | Code | Description |
|--------|------|-------------|
| `'push-int` | 0 | Push next value as integer |
| `'op` | 1 | Execute operator (next value is opcode) |
| `'return` | 2 | Return from function |
| `'push-string` | 3 | Push string from context |
| `'push-context` | 4 | Push value from context slot |
| `'pop-unused` | 5 | Discard top of stack |
| `'push-local` | 6 | Push value from frame slot |
| `'pop-local` | 7 | Pop value to frame slot |
| `'enter-frame` | 8 | Allocate frame with n slots |
| `'leave-frame` | 9 | Deallocate frame |
| `'start-array` | 10 | Begin array generator |
| `'end-array` | 11 | End array generator |
| `'call` | 12 | Call bytecodeval (sets RP) |
| `'tail-call` | 17 | Tail call bytecodeval (preserves RP) |
| `'save-return-ptr` | 13 | Push return pointer to stack |
| `'restore-return-ptr` | 14 | Pop return pointer from stack |
| `'save-context-ptr` | 15 | Push context pointer to stack |
| `'restore-context-ptr` | 16 | Pop context pointer from stack |

Track the current address manually:

```
instruction-table 'push-int @ /ocPushInt
instruction-table 'return @ /ocReturn

0 /here
[ ocPushInt 42 ocReturn ] here emit-all-at! /here
[] 0 close !               ; execute bytecode at address 0
```

Use with `operator-table` for operator codes:

```
operator-table '+ @ /opAdd
instruction-table 'op @ /ocOp

[ ocPushInt 1 ocPushInt 2 ocOp opAdd ocReturn ] 0 emit-all-at! drop!
[] 0 close !               ; returns 3
```

## Ops (ops.froth)

Operator table for bytecode generation.

| Name | Type | Description |
|------|------|-------------|
| `operator-table` | map | Map from quoted operator identifiers to codes |

The `operator-table` constant maps each operator identifier to its bytecode operator code. This is kept in sync with `src/operator_table.m`.

```
operator-table '+ @        ; 2 (code for addition)
operator-table 'print @    ; 0 (code for print)
operator-table 'close @    ; 48 (code for close)
```

Use with bytecode generation:

```
operator-table '+ @ /opAdd
[ ocOp opAdd ocReturn ] 0 emit-all-at! drop!
```

## Preflight (preflight.froth)

Pre-flight checks for static analysis and compilation.

| Name | Stack Effect | Description |
|------|--------------|-------------|
| `preflight` | `( func -- 0 \| 1 )` | Check if function is safe for static analysis |

Returns `0` if the function is safe, `1` if it contains `env`, `import`, or `applyOperator` operators which have dynamic semantics that prevent static analysis.

```
'{ x } preflight!              ; 0 (safe)
'{ /x x } preflight!           ; 0 (safe)
'{ env } preflight!            ; 1 (uses env)
'{ "foo" import } preflight!   ; 1 (uses import)
'{ '+ applyOperator } preflight! ; 1 (uses applyOperator)
'{ /x { env } } preflight!     ; 1 (nested env)
```

Use preflight before boundness analysis or compilation to ensure the function can be statically analyzed.

## Node (node.froth)

Compiler node constructors. The compiler passes (boundness, liveness, slots) produce a parallel structure of maps corresponding to each term in the input function. This module provides constructors for each node type.

| Name | Stack Effect | Description |
|------|--------------|-------------|
| `new-node` | `( -- node )` | Base constructor returning empty map |
| `new-literal-node` | `( -- node )` | Create literal node (numbers, strings) |
| `new-quote-node` | `( -- node )` | Create quote node ('symbol, 'expr) |
| `new-identifier-node` | `( -- node )` | Create identifier node |
| `new-binder-node` | `( -- node )` | Create binder node (/name) |
| `new-apply-node` | `( -- node )` | Create apply node (!) |
| `new-closure-node` | `( -- node )` | Create closure node ({ }) |
| `new-generator-node` | `( -- node )` | Create generator node ([ ]) |

**Value conventions:**
- Keys use 0 for "yes/true" and 1 for "no/false" (Froth convention)
- Defaults are set to the common case; passes only write when different
- `'body` arrays are parallel to the function body terms

**Identifier node keys:**
- `'is-bound`: 0 = bound variable, 1 = free variable
- `'is-live`: 0 = still live after this point, 1 = last reference
- `'slot`: context slot (if free) or frame slot (if bound)
- `'leave-frame`: 0 = last term using the frame

**Binder node keys:**
- `'is-used`: 0 = used somewhere, 1 = dead (never referenced)
- `'slot`: frame slot number (only set if used)

**Apply node keys:**
- `'restore-context`: 0 = non-tail call needing context restore after
- `'restore-return`: 0 = last non-tail call (needs return restore)
- `'leave-frame`: 0 = last term using the frame
- `'ctx-save-slot`: frame slot for saving context pointer (set by slots)
- `'rp-save-slot`: frame slot for saving return pointer (set by slots)

**Closure node keys:**
- `'body`: array of node maps (parallel to body terms)
- `'free-vars`: map: identifier -> context slot number
- `'bound-set`: map: identifier -> nil (set of bound vars)
- `'dead-set`: map: identifier -> nil (vars whose last use is capture)
- `'max-slots`: maximum frame slots needed
- `'needs-frame`: 0 = function needs a frame

**Generator node keys:**
- `'body`: array of node maps (parallel to body terms)
- `'free-vars`: map: identifier -> context slot number
- `'bound-set`: map: identifier -> nil (set of bound vars)

Generators share the containing function's frame, so they don't have `'max-slots`.

## Boundness (boundness.froth)

Boundness analysis for the compiler.

| Name | Stack Effect | Description |
|------|--------------|-------------|
| `boundness` | `( func -- func closure-node )` | Analyze variable binding in a function |

Analyzes a quoted function and returns the function plus a closure-node. The map contains:
- `'body`: array of node maps, one per term in the function body
- `'free-vars`: map from identifiers to context slot numbers
- `'bound-set`: map from identifiers to nil (marking presence)

Each node map contains keys appropriate to the term type (see Node module).

Context slot numbers are assigned in order of first occurrence (0, 1, 2, ...).

```
'{x} boundness! /bnd-map /func
bnd-map 'free-vars @           ; $ 0 'x : (x gets slot 0)
bnd-map 'body @ 0 @            ; identifier-node with is-bound=1 (free)

'{/x x} boundness! /bnd-map /func
bnd-map 'body @                ; [ binder-node identifier-node ]
; Second element has is-bound=0 (bound)

'{/x {x}} boundness! /bnd-map /func
bnd-map 'body @ 1 @            ; nested closure-node
; Has 'body, 'free-vars, 'bound-set keys
```

The passes compose: `func boundness! liveness! slots!` returns `(func closure-node)`.

## Liveness (liveness.froth)

Liveness analysis for the compiler.

| Name | Stack Effect | Description |
|------|--------------|-------------|
| `liveness` | `( func closure-node -- func closure-node )` | Analyze last references in function |

Takes the closure-node from `boundness` and adds liveness information.

**Per-term keys:**
- `'is-live`: `0` (still live) or `1` (last reference) for identifiers
- `'is-used`: `0` (used) or `1` (dead) for binders
- `'dead-set`: set of vars whose last use is capture (for closures only)
- `'restore-context`: `0` for non-tail calls that need context restored after
- `'restore-return`: `0` for the last non-tail call (where return pointer is restored)
- `'leave-frame`: `0` for the last term that uses the frame

**Function-level keys:**
- `'needs-frame`: `0` if function uses a frame (bound vars or register saves)

The analysis traverses right-to-left to determine which references are "last" in each scope. Key behaviors:
- Closures create new scopes; captured vars that aren't used later go in `'dead-set`
- Generators share outer scope; binders inside create local scope within generator only
- Binders report whether the variable is actually used (0) or dead (1)
- Non-tail calls track when context/return pointers need saving and restoring
- Frame exit point is marked on the last term that accesses the frame

```
'{x} boundness! liveness! /map /func
map 'body @ 0 @                ; identifier-node with is-bound=1, is-live=1
; x is free (is-bound=1) and last-ref (is-live=1)

'{/x x} boundness! liveness! /map /func
map 'body @ 0 @                ; binder-node with is-used=0
; binder is used (is-used=0)

'{/x} boundness! liveness! /map /func
map 'body @ 0 @ 'is-used @     ; 1 - binder unused (dead)

'{x {x}} boundness! liveness! /map /func
map 'body @ 1 @ 'dead-set @    ; $ . 'x :
; x's last use is this capture, so in dead-set

'{/fn fn! x fn!} boundness! liveness! /map /func
map 'body @ 2 @                ; first ! has restore-context=0, restore-return=0

'{1 2 +} boundness! liveness! /map /func
map 'needs-frame @             ; 1 - no frame needed
```

## Slots (slots.froth)

Frame slot allocation for the compiler.

| Name | Stack Effect | Description |
|------|--------------|-------------|
| `slots` | `( func closure-node -- func closure-node )` | Allocate frame slots |

Takes the closure-node from `liveness` and adds slot allocation information:

- `'slot`: frame slot number (for live binders and bound identifier references)
- `'ctx-save-slot`: frame slot for saving context pointer (for non-tail calls)
- `'rp-save-slot`: frame slot for saving return pointer (for non-tail calls)
- `'max-slots`: maximum number of slots needed (for closures only)

Slots are allocated on-demand and reused when freed. Binder slots are freed at the variable's last use. Register save slots are allocated at non-tail calls and freed immediately after. Closures get their own frame (slots start at 0), while generators share the outer frame.

```
'{/x x} boundness! liveness! slots! /map /func
map 'max-slots @               ; 1 - one slot needed
map 'body @ 0 @ 'slot @        ; 0 - binder gets slot 0
map 'body @ 1 @ 'slot @        ; 0 - reference uses slot 0

'{/x x /y y} boundness! liveness! slots! /map /func
map 'max-slots @               ; 1 - slot 0 reused for y

'{/x {/y y} x} boundness! liveness! slots! /map /func
map 'max-slots @               ; 1 - outer needs 1 slot
map 'body @ 1 @ 'max-slots @   ; 1 - closure needs 1 slot

'{/x /y [/z z] y x} boundness! liveness! slots! /map /func
map 'max-slots @               ; 3 - generator uses slot 2 for z

'{/x x x * f! 1 +} boundness! liveness! slots! /map /func
map 'max-slots @               ; 1 - slot 0 reused for RP save
map 'body @ 5 @ 'rp-save-slot @ ; 0 - apply saves RP to slot 0
```
