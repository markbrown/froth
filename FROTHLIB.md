# Froth Standard Library Reference

The standard library (`lib/stdlib.froth`) loads automatically unless `-n` is given. It imports the following modules:

## Definitions (defs.froth)

Utilities for creating closures with minimal environments. This module is imported first and has no dependencies on other stdlib modules.

| Name | Stack Effect | Description |
|------|--------------|-------------|
| `def-fn` | `( deps closure -- closure )` | Create closure with environment restricted to deps |

When a closure is created with `{ ... }`, it captures the entire current environment. This can cause environment bloat to propagate through a chain of definitions. Use `def-fn` to explicitly specify which bindings a closure needs:

```
{ 1 } /a
{ a! 1 + } /b

; Without def-fn: c captures full env (47+ bindings)
{ b! 1 + } /c-full
c-full closureEnv #           ; 47

; With def-fn: c captures only b (1 binding)
['b] { b! 1 + } def-fn! /c-lean
c-lean closureEnv #           ; 1

; Both produce the same result
c-full!                       ; 3
c-lean!                       ; 3
```

This is definition-time optimization: later definitions that capture `c-lean` will get the minimal version, preventing transitive environment bloat.

## I/O Utilities (io.froth)

| Name | Stack Effect | Description |
|------|--------------|-------------|
| `nl` | `( -- )` | Print a newline |
| `println` | `( a -- )` | Print a value followed by a newline |
| `writeln` | `( a -- )` | Write in executable form followed by a newline |

## Array (array.froth)

| Name | Stack Effect | Description |
|------|--------------|-------------|
| `foldl` | `( arr fn -- ... )` | Apply fn to each element left-to-right |
| `foldr` | `( arr fn -- ... )` | Apply fn to each element right-to-left |
| `map` | `( arr fn -- arr )` | Apply fn to each element, return new array |
| `filter` | `( arr fn -- arr )` | Keep elements where fn returns 0 |
| `reduce` | `( arr init fn -- val )` | Reduce array with binary function |
| `concat` | `( arr1 arr2 -- arr )` | Concatenate two arrays |
| `flatten` | `( arr-of-arrs -- arr )` | Flatten nested arrays |
| `reverse` | `( arr -- arr )` | Reverse an array |

The function may leave zero or more values on the stack per element. Use a generator to collect results into an array.

```
[1 2 3] {1 +} foldl!           ; leaves 2 3 4 on stack
[ [1 2 3] {1 +} foldl! ]       ; produces [2 3 4]
[1 2 3] {print} foldr!         ; prints 3, 2, 1
```

## List (list.froth)

Utilities for cons lists. Lists are built with `.` (nil) and `,` (cons):

```
.              ; empty list
. 3 , 2 , 1 ,  ; list [1, 2, 3] (1 is head)
```

| Name | Stack Effect | Description |
|------|--------------|-------------|
| `lfoldl` | `( list fn -- ... )` | Apply fn to each element head-to-tail |
| `lfoldr` | `( list fn -- ... )` | Apply fn to each element tail-to-head |
| `lreverse` | `( list -- list )` | Reverse a list |

```
. 3 , 2 , 1 , {print} lfoldl!   ; prints 1, 2, 3
. 3 , 2 , 1 , {print} lfoldr!   ; prints 3, 2, 1
. 3 , 2 , 1 , lreverse!         ; produces [3, 2, 1]
0 . 3 , 2 , 1 , {+} lfoldl!     ; sums to 6
```

## Eval (eval.froth)

A meta-interpreter that evaluates Froth closures.

| Name | Stack Effect | Description |
|------|--------------|-------------|
| `eval` | `( stack op-table closure -- result-stack 0 \| error-msg 1 )` | Evaluate closure with initial stack and operator table |

The stack is represented as a cons list (nil-terminated). Push with `, `:

```
.                  ; empty stack (nil)
. 3 ,              ; stack with just 3
. 3 , 2 , 1 ,      ; stack [1, 2, 3] (1 on top)
```

The operator table is a map from quoted identifiers to closures. Custom operators are auto-applied (no `!` needed) when their name is encountered:

```
$ {2 *} 'double :              ; op-table with 'double' operator
. ops { 5 double } eval!       ; returns (. 10 ,) 0
```

Returns two values: `result-stack` and `0` on success, or `error-message` and `1` on error.

```
. $ { 3 4 + } eval!            ; returns (. 7 ,) 0
. $ { 10 /x x 1 + } eval!      ; returns (. 11 ,) 0
. 10 , $ { 5 + } eval!         ; returns (. 15 ,) 0
```

Supported: all operators (dispatched by arity), variable binding, function literals, function application (closures and quoted operators), quoted terms, generators, `env`, `stack`, custom operators via op-table.

Not yet supported: I/O operators.

## Map (map.froth)

Map utilities.

| Name | Stack Effect | Description |
|------|--------------|-------------|
| `restrict` | `( map keys -- map )` | Restrict map to only keys in array |
| `transform-values` | `( map fn -- map )` | Transform each value in map using fn |

```
$ 1 'a : 2 'b : 3 'c : /m
m ['a 'c] restrict!            ; $ 1 'a : 3 'c :
m { 2 * } transform-values!    ; $ 2 'a : 4 'b : 6 'c :
```

## Data (data.froth)

Generic data structure utilities.

| Name | Stack Effect | Description |
|------|--------------|-------------|
| `transform` | `( data fn -- data )` | Recursively transform data structure |

The `transform` function applies a function to all leaf values in a data structure, recursively traversing into maps, arrays, and cons cells. Nil values are left unchanged.

```
[ 1 2 3 ] { 2 * } transform!           ; [ 2 4 6 ]
$ 1 'a : 2 'b : { 10 + } transform!    ; $ 11 'a : 12 'b :
. 3 , 2 , 1 , { 2 * } transform!       ; . 6 , 4 , 2 ,

; Works on nested structures
[ [ 1 2 ] [ 3 4 ] ] { 2 * } transform! ; [ [ 2 4 ] [ 6 8 ] ]
$ [ 1 2 ] 'arr : { 2 * } transform!    ; $ [ 2 4 ] 'arr :
```

## Boolean (bool.froth)

Boolean operations where 0 represents true and non-zero represents false.

| Name | Stack Effect | Description |
|------|--------------|-------------|
| `not` | `( a -- int )` | 0 if a is non-zero or non-integer, else 1 |
| `and` | `( a b -- int )` | 0 if both a and b are 0, else 1 |
| `or` | `( a b -- int )` | 0 if either a or b is 0, else 1 |

```
0 not!             ; 1 (not true = false)
1 not!             ; 0 (not false = true)
"x" not!           ; 0 (non-integer = true)
0 0 and!           ; 0 (true and true)
0 1 and!           ; 1 (true and false)
0 1 or!            ; 0 (true or false)
1 1 or!            ; 1 (false or false)
```

## Math (math.froth)

Mathematical utilities.

| Name | Stack Effect | Description |
|------|--------------|-------------|
| `fib` | `( a b n -- vals... )` | Generate Fibonacci sequence of length n |

The `fib` function generates a Fibonacci sequence starting with the two given values, leaving n values on the stack.

```
[ 0 1 8 fib! ]     ; [ 0 1 1 2 3 5 8 13 ]
[ 1 1 5 fib! ]     ; [ 1 1 2 3 5 ]
[ 2 3 4 fib! ]     ; [ 2 3 5 8 ]
```

## Bench (bench.froth)

Benchmarking utilities.

| Name | Stack Effect | Description |
|------|--------------|-------------|
| `bench` | `( closure n -- ticks )` | Execute closure n times, return elapsed clock ticks |

Values produced by the closure are discarded after all iterations.

```
{ 1 2 + } 10000 bench! println!     ; prints elapsed ticks
{ 0 1 20 fib! } 1000 bench! println!
```

## Analysis (analysis.froth)

Static analysis utilities for Froth code.

| Name | Stack Effect | Description |
|------|--------------|-------------|
| `free-vars` | `( term -- array 0 \| 1 )` | Get free variables in a term |

Returns `(array 0)` on success with array of free identifiers, or `1` if the term uses `env` or `import` (which prevent closure optimization).

```
'{ x } free-vars!              ; returns ['x] 0
'{ /x x } free-vars!           ; returns [] 0 (x is bound)
'{ /x y } free-vars!           ; returns ['y] 0
'{ env } free-vars!            ; returns 1 (uses env)
'{ "foo" import } free-vars!   ; returns 1 (uses import)
```

Free variable analysis:
- Identifiers are free if not bound by a binder and not operators
- Quoted terms (`'x`) are data, not references, so not counted
- Nested functions inherit outer bindings
- `env` and `import` prevent optimization (return 1)

## Optimize (optimize.froth)

Closure optimization utilities.

| Name | Stack Effect | Description |
|------|--------------|-------------|
| `restrict-closure-env` | `( closure -- closure 0 \| 1 )` | Restrict closure environment to free variables |
| `optimize` | `( data -- data )` | Recursively optimize all closures in a data structure |

Restricts a closure's captured environment to only the variables that are actually used (free) in the function body. Returns `(optimized-closure 0)` on success, or `1` if free variable analysis failed (due to `env` or `import` usage).

```
1 /a 2 /b { a } /f        ; f captures both a and b
f closureEnv #             ; 2 (or more with stdlib)
f restrict-closure-env!
{ /opt
  opt closureEnv #         ; 1 (only 'a)
  opt!                     ; 1 (still works)
} { "failed" } ?!
```

The `optimize` function uses `transform` to traverse any data structure (arrays, maps, cons cells) and optimize all closures within it. For each closure:
1. Restricts the environment to only free variables
2. Recursively optimizes the closure's environment

```
1 /unused 2 /used
{ used } /f
[ f f ] optimize!          ; array with optimized closures
$ f 'fn : optimize!        ; map with optimized closure value
```

To optimize all closures in the current environment, use inline (not in a closure, due to lexical scoping):

```
env optimize! restore      ; optimize and replace current environment
```
