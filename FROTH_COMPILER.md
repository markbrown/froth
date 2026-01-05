# Froth Compiler Reference

This document describes the compiler infrastructure for Froth, including bytecode generation and static analysis passes.

## Quick Reference

| Name | Module | Description |
|------|--------|-------------|
| `boundness` | boundness | Analyze variable binding in a node |
| `compile` | compile | Compile a binding in an env-map |
| `compile-func` | compile | Build node tree and run all analysis passes |
| `compile-named-closure` | compile | Compile a closure to bytecode with caching |
| `emit-all-at` | bytecode | Write array to bytecode store |
| `emit-at` | bytecode | Write value to bytecode store |
| `liveness` | liveness | Analyze last references in a node |
| `operator-table` | ops | Constant: map from operator identifiers to codes |
| `instruction-table` | bytecode | Constant: map from symbols to VM instruction codes |
| `new-apply-node` | node | Create apply node with defaults |
| `new-binder-node` | node | Create binder node with defaults |
| `new-function-node` | node | Create function node with defaults |
| `new-generator-node` | node | Create generator node with defaults |
| `new-identifier-node` | node | Create identifier node with defaults |
| `new-literal-node` | node | Create literal node with defaults |
| `new-node` | node | Create base node with term |
| `make-node` | node | Create appropriate node type for term |
| `new-quote-node` | node | Create quote node with defaults |
| `preflight` | preflight | Check for env/import/applyOperator usage |
| `slots` | slots | Allocate frame slots for function |
| `codegen` | codegen | Generate bytecode for a function |

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
| `'push-quoted-apply` | 18 | Push quoted apply term (`'!`) |

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
| `new-node` | `(term -- node)` | Base constructor storing the term |
| `new-literal-node` | `(term -- node)` | Create literal node (numbers, strings) |
| `new-quote-node` | `(term -- node)` | Create quote node ('symbol, 'expr) |
| `new-identifier-node` | `(term -- node)` | Create identifier node (variables and operators) |
| `new-binder-node` | `(term -- node)` | Create binder node (/name) |
| `new-apply-node` | `(term -- node)` | Create apply node (!) |
| `new-function-node` | `(term -- node)` | Create function node ({ }) |
| `new-generator-node` | `(term -- node)` | Create generator node ([ ]) |
| `make-node` | `(term -- node)` | Create appropriate node type based on term |

**Value conventions:**
- All nodes have `'term` key storing the original syntax term
- Keys use 0 for "yes/true" and 1 for "no/false" (Froth convention)
- Defaults are set to the common case; passes only write when different
- `'body` arrays contain child nodes (one per term)

**Identifier node keys:**
Both variables and operators use identifier nodes. The `'is-operator` key distinguishes them:
- `'is-operator`: 0 = operator (looked up in operator-table), 1 = variable (default)
- `'is-bound`: 0 = bound variable, 1 = free variable (only meaningful when `'is-operator` = 1)
- `'is-live`: 0 = still live after this point, 1 = last reference
- `'slot`: context slot (if free) or frame slot (if bound)
- `'leave-frame`: 0 = last term using the frame

The `'is-operator` key is set by the boundness pass. Operators that are shadowed by a binder (e.g., `+` in `{/+ x + y}`) have `'is-operator` = 1 and are treated as bound variables.

**Binder node keys:**
- `'is-used`: 0 = used somewhere, 1 = dead (never referenced)
- `'slot`: frame slot number (only set if used)

**Apply node keys:**
- `'is-tail-call`: 0 = tail call, 1 = non-tail call
- `'restore-context`: 0 = non-tail call needing context restore after
- `'restore-return`: 0 = last non-tail call (needs return restore)
- `'leave-frame`: 0 = last term using the frame
- `'save-context`: 0 = first call needing context save (set by slots)
- `'save-return`: 0 = first non-tail call needing return save (set by slots)
- `'ctx-save-slot`: frame slot for saving context pointer (set by slots)
- `'rp-save-slot`: frame slot for saving return pointer (set by slots)

**Function node keys:**
- `'body`: array of node maps (parallel to body terms)
- `'free-vars-map`: map: identifier -> context slot number
- `'free-vars-array`: array: slot index -> identifier
- `'bound-set`: map: identifier -> nil (set of bound vars)
- `'dead-set`: map: identifier -> nil (vars whose last use is capture)
- `'max-slots`: maximum frame slots needed
- `'needs-frame`: 0 = function needs a frame

**Generator node keys:**
- `'body`: array of node maps (parallel to body terms)
- `'bound-set`: map: identifier -> nil (set of bound vars)

Generators share the containing function's frame and context, so they don't have `'max-slots`, `'free-vars-map`, or `'free-vars-array`.

## Boundness (boundness.froth)

Boundness analysis for the compiler.

| Name | Stack Effect | Description |
|------|--------------|-------------|
| `boundness` | `( func-node -- func-node )` | Analyze variable binding in a function node |

Takes a function-node (from `compile-func`) with `'term` and `'body` keys, and adds boundness information. Does not recurse into nested functions (they are already analyzed by `compile-func`), but does recurse into generators (which share the outer scope).

Adds these keys to the function-node:
- `'free-vars-map`: map from identifiers to context slot numbers
- `'free-vars-array`: array of identifiers indexed by context slot
- `'bound-set`: map from identifiers to nil (marking presence)

Updates child nodes with:
- `'is-operator`: 0 = operator, 1 = variable (for identifiers not shadowed by binders)
- `'is-bound`: 0 = bound variable, 1 = free variable (for variable identifiers)
- `'slot`: context slot number (for free identifiers)

Context slot numbers are assigned in order of first occurrence (0, 1, 2, ...).

When called via `compile-func`, the passes compose automatically. For direct use:

```
'{ /x x } make-node! /node
node [ '{ /x x } { make-node! } foldl! ] 'body : boundness! /bnd-map
bnd-map 'free-vars-map @           ; $ (empty - no free vars)
bnd-map 'bound-set @               ; $ . 'x : (x is bound)
```

## Liveness (liveness.froth)

Liveness analysis for the compiler.

| Name | Stack Effect | Description |
|------|--------------|-------------|
| `liveness` | `( func-node -- func-node )` | Analyze last references in function node |

Takes a function-node (from `compile-func` after `boundness`) and adds liveness information. Does not recurse into nested functions (they are already analyzed), but does recurse into generators (which share the outer scope).

**Per-term keys:**
- `'is-live`: `0` (still live) or `1` (last reference) for identifiers
- `'is-used`: `0` (used) or `1` (dead) for binders
- `'dead-set`: set of vars whose last use is capture (for closures only)
- `'is-tail-call`: `0` for apply in tail position (last term in function)
- `'restore-context`: `0` for non-tail calls that need context restored after
- `'restore-return`: `0` for the last non-tail call (where return pointer is restored)
- `'leave-frame`: `0` for the last term that uses the frame

**Function-level keys:**
- `'needs-frame`: `0` if function uses a frame (bound vars or register saves)

The analysis traverses right-to-left to determine which references are "last" in each scope. Key behaviors:
- For nested functions, computes `'dead-set` (captured vars not used later in outer scope)
- Generators share outer scope; binders inside create local scope within generator only
- Binders report whether the variable is actually used (0) or dead (1)
- Non-tail calls track when context/return pointers need saving and restoring
- Frame exit point is marked on the last term that accesses the frame

When called via `compile-func`, the passes compose automatically.

## Slots (slots.froth)

Frame slot allocation for the compiler.

| Name | Stack Effect | Description |
|------|--------------|-------------|
| `slots` | `( func-node -- func-node )` | Allocate frame slots |

Takes the function-node from `liveness` and adds slot allocation information.

- `'slot`: frame slot number (for live binders and bound identifier references)
- `'save-context`: 0 on first call needing context save
- `'save-return`: 0 on first non-tail call needing return save
- `'ctx-save-slot`: frame slot for saving context pointer (for calls needing context restore)
- `'rp-save-slot`: frame slot for saving return pointer (for non-tail calls)
- `'max-slots`: maximum number of slots needed (for closures only)

Slots are allocated on-demand and reused when freed. Binder slots are freed at the variable's last use. Register save slots are allocated once at the first call that needs them and reused by subsequent calls. Closures get their own frame (slots start at 0), while generators share the outer frame.

```
tree-empty! 0 '{/x x} compile-func! /node /addr /code-map
node 'max-slots @               ; 1 - one slot needed
node 'body @ 0 @ 'slot @        ; 0 - binder gets slot 0
node 'body @ 1 @ 'slot @        ; 0 - reference uses slot 0

tree-empty! 0 '{/x x /y y} compile-func! /node /addr /code-map
node 'max-slots @               ; 1 - slot 0 reused for y

tree-empty! 0 '{/x {/y y} x} compile-func! /node /addr /code-map
node 'max-slots @               ; 1 - outer needs 1 slot
node 'body @ 1 @ 'max-slots @   ; 1 - closure needs 1 slot

tree-empty! 0 '{/x /y [/z z] y x} compile-func! /node /addr /code-map
node 'max-slots @               ; 3 - generator uses slot 2 for z

tree-empty! 0 '{/x x x * f! 1 +} compile-func! /node /addr /code-map
node 'max-slots @               ; 1 - slot 0 reused for RP save
node 'body @ 5 @ 'rp-save-slot @ ; 0 - apply saves RP to slot 0
```

When called via `compile-func`, the passes compose automatically.

## Codegen (codegen.froth)

Bytecode generation for the compiler.

| Name | Stack Effect | Description |
|------|--------------|-------------|
| `codegen` | `(addr func-node -- next-addr func-node)` | Generate bytecode for a function |

Takes a function node containing the term and analysis data, generates bytecode at `addr`, and returns the next available address and the updated func-node with `'func-addr` stored.

- `addr`: Next available bytecode address
- `func-node`: Node containing `'term` (quoted function) and analysis results from boundness/liveness/slots
- `next-addr`: Next available address after code is emitted
- `func-node` (returned): Updated with `'func-addr` set to this function's bytecode entry point

Supports:
- Integer and string literals (`push-int`, `push-string`)
- Operators (`op`)
- Free variables (`push-context`)
- Bound variables (`enter-frame`, `pop-local`, `push-local`, `leave-frame`)
- Dead binders (`pop-unused`)
- Quoted identifiers, binders, apply, and values
- Tail calls (`tail-call` instead of `return`)
- Non-tail calls (`call` with register save/restore)
- Nested functions (reads `'func-addr` from pre-compiled nodes)
- Generators (recurses into body, shares outer frame)

```
; Compile a simple function
tree-empty! 0 '{ 1 2 3 } compile-func! /node /addr /code-map
addr node codegen! /node /next-addr
node 'func-addr @              ; bytecode entry point
; Emits: push-int 1 push-int 2 push-int 3 return

; Compile with bound variables
tree-empty! 0 '{ /x x } compile-func! /node /addr /code-map
addr node codegen! /node /next-addr
; Emits: enter-frame 1 pop-local 0 push-local 0 leave-frame 1 return
```

## Compile (compile.froth)

Compiler orchestration. Builds the node tree and runs all analysis passes.

| Name | Stack Effect | Description |
|------|--------------|-------------|
| `compile` | `( cache code-map addr env-map name -- cache code-map addr env-map )` | Compile a binding in env-map |
| `compile-func` | `( code-map addr func -- code-map addr func-node )` | Compile a function |
| `compile-named-closure` | `( cache code-map addr closureval name -- cache code-map addr bytecodeval )` | Compile a closure to bytecode |

Takes a quoted function and returns a fully analyzed function-node. Recursively compiles nested functions before the outer function, so each function's analysis can assume nested functions are already complete.

- `code-map`: Tree23 for deduplication (maps function ref IDs to analyzed nodes)
- `addr`: Next available bytecode address (threaded through for future codegen)
- `func`: Quoted function to compile
- `func-node`: Fully analyzed node with boundness, liveness, and slots info

The compilation pipeline for each function:
1. Build node tree (creates nodes for each term, recurses into nested functions)
2. Run `boundness` (classify variables as bound/free, assign context slots)
3. Run `liveness` (mark last references, tail calls, register save/restore points)
4. Run `slots` (allocate frame slots with reuse)

Nested functions are compiled bottom-up: when processing a nested function term, `compile-func` recursively compiles it before continuing with the outer function. This means the analysis passes don't need to recurse into nested functions - they just read the already-computed results.

```
tree-empty! 0 '{ /x x } compile-func! /node /addr /code-map
node 'max-slots @              ; 1
node 'body @ 0 @ 'slot @       ; 0 (binder slot)
node 'body @ 1 @ 'slot @       ; 0 (reference slot)

; Nested functions are compiled first
tree-empty! 0 '{ /x { x } } compile-func! /node /addr /code-map
node 'body @ 1 @ 'free-vars-map @  ; $ 0 'x : (nested func captured x)
```

### compile-named-closure

Compiles a closureval to a bytecodeval, recursively compiling any captured closures.

- `cache`: Cache for compiled closures (from `cache-empty!`)
- `code-map`: Tree23 for function deduplication
- `addr`: Next available bytecode address
- `closureval`: The closure to compile
- `name`: Quoted identifier for cache lookup
- `bytecodeval`: The compiled closure (array + bytecode address)

The function:
1. Checks the cache for an already-compiled version
2. Opens the closure to get its body (quoted function) and environment
3. Compiles the function body using `compile-func`
4. For each free variable binding from the closure's environment:
   - If the value is an uncompiled closureval, recursively compiles it
   - If it's already a bytecodeval or non-closure, uses it as-is
5. Creates the bytecodeval with the context array and bytecode address
6. Caches the result for future lookups

```
; Simple closure
{ 1 2 + } /f
cache-empty! tree-empty! 0 f 'f compile-named-closure! /bv /addr /code-map /cache
bv!                            ; 3

; Closure capturing another closure (recursive compilation)
{ 10 } /g
{ g! 1 + } /f
cache-empty! tree-empty! 0 f 'f compile-named-closure! /bv /addr /code-map /cache
bv!                            ; 11 (g is compiled automatically)

; Cache reuse
cache code-map addr f 'f compile-named-closure! /bv2 /addr2 /code-map /cache
addr addr2 =                   ; 0 (same address, cache hit)
```

### compile

Convenience wrapper that looks up a binding in an env-map, compiles it, and updates the map.

- `cache`: Cache for compiled closures
- `code-map`: Tree23 for function deduplication
- `addr`: Next available bytecode address
- `env-map`: Map from identifiers to closures
- `name`: Quoted identifier to compile

```
{ 1 2 + } /f
{ 10 } /g
$ f 'f : g 'g : /env-map

cache-empty! tree-empty! 0 env-map 'f compile! /env-map /addr /code-map /cache
env-map 'f @ !                 ; 3 (compiled and executed)

cache code-map addr env-map 'g compile! /env-map /addr /code-map /cache
env-map 'g @ !                 ; 10
```
