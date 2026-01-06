# Froth! Standard Library Reference

The standard library (`lib/stdlib.froth`) loads automatically unless `-n` is given. It imports the following modules:

## Quick Reference

| Name | Module | Description |
|------|--------|-------------|
| `add-keys` | map | Add array of keys with nil values |
| `alist-delete` | alist | Remove key from alist |
| `alist-empty` | alist | Check if alist is empty |
| `alist-find` | alist | Find pair by key |
| `alist-get` | alist | Get value by key |
| `alist-has` | alist | Check if key exists |
| `alist-keys` | alist | Get all keys as array |
| `alist-set` | alist | Set or update key-value pair |
| `alist-size` | alist | Get number of entries |
| `alist-values` | alist | Get all values as array |
| `and` | bool | Logical and |
| `bench` | bench | Benchmark closure execution |
| `concat` | array | Concatenate two arrays (copies both) |
| `contains` | array | Check if array contains element |
| `def-fn` | defs | Create closure with minimal environment |
| `delete-keys` | map | Delete array of keys from map |
| `drop` | stack | Drop top of stack |
| `dup` | stack | Duplicate top of stack |
| `eval` | eval | Evaluate closure with stack and op-table |
| `fact` | math | Compute factorial |
| `fib` | math | Generate Fibonacci sequence |
| `filter` | array | Keep elements matching predicate |
| `flatten` | array | Flatten nested arrays |
| `foldl` | array | Apply fn to elements left-to-right |
| `foldr` | array | Apply fn to elements right-to-left |
| `lconcat` | list | Concatenate two lists |
| `lfoldl` | list | Apply fn head-to-tail |
| `lfoldr` | list | Apply fn tail-to-head |
| `lmap` | list | Apply fn to each element |
| `lreverse` | list | Reverse a cons list |
| `map` | array | Transform each element |
| `merge` | map | Merge two maps (map2 takes precedence) |
| `nl` | io | Print newline |
| `nip` | stack | Remove second element from stack |
| `not` | bool | Logical not (0→1, else→0) |
| `or` | bool | Logical or |
| `over` | stack | Copy second element to top |
| `println` | io | Print value with newline |
| `reduce` | array | Reduce with binary function |
| `restrict` | map | Restrict map to specified keys |
| `reverse` | array | Reverse an array |
| `scanl` | array | Iterate left-to-right until predicate returns 0 |
| `scanr` | array | Iterate right-to-left until predicate returns 0 |
| `swap` | stack | Swap top two elements |
| `times` | control | Execute body n times |
| `times-loop` | control | Execute body n times with state |
| `transform` | data | Recursively transform data structure |
| `transform-values` | map | Transform each value in map |
| `tree-empty` | tree23 | Create an empty tree |
| `tree-fold` | tree23 | Apply fn to each key-value pair in order |
| `tree-get` | tree23 | Get value by key |
| `tree-has` | tree23 | Check if key exists |
| `tree-is-empty` | tree23 | Check if tree is empty |
| `tree-keys` | tree23 | Get all keys as array (in order) |
| `tree-set` | tree23 | Set or update key-value pair |
| `tree-size` | tree23 | Get number of entries |
| `tree-values` | tree23 | Get all values as array (in key order) |
| `until` | control | Execute body until condition is true |
| `until-loop` | control | Loop until condition with state |
| `while` | control | Execute body while condition is true |
| `while-loop` | control | Loop while condition with state |
| `writeln` | io | Write in executable form with newline |

Unlike operators, functions must be followed by `!` to be applied (e.g., `println!`, `foldl!`).

## Definitions (defs.froth)

Utilities for creating closures with minimal environments. This module is imported first and has no dependencies on other stdlib modules.

| Name | Stack Effect | Description |
|------|--------------|-------------|
| `def-fn` | `( deps closure -- closure )` | Create closure with environment restricted to deps |

When a closureval is created with `{ ... }`, it captures the entire current environment. This can cause environment bloat to propagate through a chain of definitions. Use `def-fn` to explicitly specify which bindings a closure needs:

```
{ 1 } /a
{ a! 1 + } /b

; Without def-fn: c captures full env (many bindings)
{ b! 1 + } /c-full
c-full open /body /env env #  ; many bindings

; With def-fn: c captures only b (1 binding)
['b] { b! 1 + } def-fn! /c-lean
c-lean open /body /env env #  ; 1

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
| `concat` | `( arr1 arr2 -- arr )` | Concatenate two arrays (copies both, O(n+m)) |
| `flatten` | `( arr-of-arrs -- arr )` | Flatten nested arrays |
| `reverse` | `( arr -- arr )` | Reverse an array |
| `scanl` | `( arr fn -- 0 \| 1 )` | Iterate left-to-right until fn returns 0 |
| `scanr` | `( arr fn -- 0 \| 1 )` | Iterate right-to-left until fn returns 0 |
| `contains` | `( arr x -- 0 \| 1 )` | Check if array contains element |

The function may leave zero or more values on the stack per element. Use a generator to collect results into an array.

```
[1 2 3] {1 +} foldl!           ; leaves 2 3 4 on stack
[ [1 2 3] {1 +} foldl! ]       ; produces [2 3 4]
[1 2 3] {print} foldr!         ; prints 3, 2, 1
```

The `scanl` and `scanr` functions iterate until the predicate returns 0 (found):

```
[1 2 3] {3 =} scanl!           ; 0 (found 3)
[1 2 3] {5 =} scanl!           ; 1 (not found)
[1 2 3] 2 contains!            ; 0 (found)
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
| `lconcat` | `( list1 list2 -- list )` | Concatenate two lists |
| `lmap` | `( list fn -- list )` | Apply fn to each element, return new list |

```
. 3 , 2 , 1 , {print} lfoldl!   ; prints 1, 2, 3
. 3 , 2 , 1 , {print} lfoldr!   ; prints 3, 2, 1
. 3 , 2 , 1 , lreverse!         ; produces [3, 2, 1]
0 . 3 , 2 , 1 , {+} lfoldl!     ; sums to 6
. 2 , 1 , . 4 , 3 , lconcat!    ; produces [1, 2, 3, 4]
. 3 , 2 , 1 , {1 +} lmap!       ; produces [2, 3, 4]
```

## Eval (eval.froth)

A meta-interpreter that evaluates Froth! closures.

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
| `merge` | `( map1 map2 -- map )` | Merge maps, map2 values take precedence |
| `delete-keys` | `( map keys -- map )` | Delete array of keys from map |
| `add-keys` | `( map keys -- map )` | Add keys with nil values (set union) |

```
$ 1 'a : 2 'b : 3 'c : /m
m ['a 'c] restrict!            ; $ 1 'a : 3 'c :
m { 2 * } transform-values!    ; $ 2 'a : 4 'b : 6 'c :
$ 1 'a : 2 'b : $ 3 'b : 4 'c : merge!  ; $ 1 'a : 3 'b : 4 'c :
m ['a 'c] delete-keys!         ; $ 2 'b :
$ ['x 'y] add-keys!            ; $ . 'x : . 'y :
```

## Alist (alist.froth)

Association lists for key-value storage with arbitrary key types. Unlike maps (which only support identifier keys), alists can use any value as a key. Insert is O(1), lookup/update/delete are O(n).

An alist is a cons list of `[key value]` pairs:

```
.                              ; empty alist
. [1 "one"] , [2 "two"] ,      ; alist with integer keys (2 is head)
. ["a" 1] , ["b" 2] ,          ; alist with string keys
```

| Name | Stack Effect | Description |
|------|--------------|-------------|
| `alist-find` | `( alist key -- pair 0 \| 1 )` | Find pair by key |
| `alist-get` | `( alist key -- value 0 \| 1 )` | Get value by key |
| `alist-has` | `( alist key -- 0 \| 1 )` | Check if key exists |
| `alist-set` | `( alist key value -- alist' )` | Set or update key-value pair |
| `alist-delete` | `( alist key -- alist' )` | Remove key from alist |
| `alist-keys` | `( alist -- array )` | Get all keys as array |
| `alist-values` | `( alist -- array )` | Get all values as array |
| `alist-size` | `( alist -- n )` | Get number of entries |
| `alist-empty` | `( alist -- 0 \| 1 )` | Check if empty (0 = yes) |

Lookup functions return a success flag (0 = found, 1 = not found):

```
. [1 "one"] , [2 "two"] , /a
a 1 alist-get!                 ; "one" 0 (found)
a 3 alist-get!                 ; 1 (not found)
a 2 alist-has!                 ; 0 (exists)
```

Mutation functions return a new alist (immutable updates). Insert prepends (O(1)), update and delete rebuild the list:

```
. /a
a 1 "one" alist-set! /a        ; . [1 "one"] ,
a 2 "two" alist-set! /a        ; . [1 "one"] , [2 "two"] ,
a 1 "ONE" alist-set! /a        ; . [1 "ONE"] , [2 "two"] , (updated)
a 1 alist-delete! /a           ; . [2 "two"] ,
```

## Tree23 (tree23.froth)

A 2-3 tree for balanced key-value storage with O(log n) operations. Keys must be integers; use `ref` to convert other values to integer keys.

| Name | Stack Effect | Description |
|------|--------------|-------------|
| `tree-empty` | `( -- tree )` | Create an empty tree |
| `tree-is-empty` | `( tree -- 0 \| 1 )` | Check if empty (0 = yes) |
| `tree-get` | `( tree key -- value 0 \| 1 )` | Get value by key |
| `tree-has` | `( tree key -- 0 \| 1 )` | Check if key exists |
| `tree-set` | `( tree key value -- tree' )` | Set or update key-value pair |
| `tree-size` | `( tree -- n )` | Get number of entries |
| `tree-keys` | `( tree -- array )` | Get all keys as array (in order) |
| `tree-values` | `( tree -- array )` | Get all values as array (in key order) |
| `tree-fold` | `( tree fn -- ... )` | Apply fn to each (key value) pair in order |

Lookup functions return a success flag (0 = found, 1 = not found):

```
tree-empty! /t
t 10 "ten" tree-set! /t
t 5 "five" tree-set! /t
t 15 "fifteen" tree-set! /t

t 10 tree-get!                 ; "ten" 0 (found)
t 7 tree-get!                  ; 1 (not found)
t 5 tree-has!                  ; 0 (exists)
t tree-size!                   ; 3
```

Keys are stored in sorted order:

```
t tree-keys!                   ; [5 10 15]
t tree-values!                 ; ["five" "ten" "fifteen"]
t { /v /k k print " " print v println! } tree-fold!
; prints: 5 five, 10 ten, 15 fifteen
```

Use `ref` to map arbitrary values to integer keys:

```
tree-empty! /t
"hello" ref /k1
"world" ref /k2
t k1 100 tree-set! /t
t k2 200 tree-set! /t
t k1 tree-get!                 ; 100 0
```

## Stack (stack.froth)

Stack manipulation primitives.

| Name | Stack Effect | Description |
|------|--------------|-------------|
| `swap` | `( a b -- b a )` | Swap top two elements |
| `dup` | `( a -- a a )` | Duplicate top element |
| `over` | `( a b -- a b a )` | Copy second element to top |
| `drop` | `( a -- )` | Drop top element |
| `nip` | `( a b -- b )` | Remove second element |

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

## Control (control.froth)

Control flow utilities for loops and iteration. State is passed through the stack because Froth! closures capture their environment at creation time, and bindings don't escape closure boundaries.

| Name | Stack Effect | Description |
|------|--------------|-------------|
| `while-loop` | `( state cond body -- state' )` | Loop while condition is true, with state |
| `until-loop` | `( state cond body -- state' )` | Loop until condition is true, with state |
| `times-loop` | `( state n body -- state' )` | Execute body n times, with state |
| `times` | `( n body -- )` | Execute body n times |
| `while` | `( cond body -- )` | Execute body while condition is true |
| `until` | `( cond body -- )` | Execute body until condition is true |

The `-loop` variants pass state through the stack and are the recommended way to write loops:

```
; Count from 0 to 4
0 { dup! 5 < } { dup! print " " print 1 + } while-loop! drop! nl!
; Output: 0 1 2 3 4

; Count until reaching 5
0 { dup! 5 = } { dup! print " " print 1 + } until-loop! drop! nl!
; Output: 0 1 2 3 4

; Sum 1 to 5
0 5 { 1 + } times-loop!    ; Result: 5

; Print "x" five times
5 { "x" print } times! nl! ; Output: xxxxx
```

For `-loop` variants:
- `cond`: `( state -- state flag )` - examines state, returns flag (0 = continue for while-loop, 0 = stop for until-loop)
- `body`: `( state -- state' )` - transforms state

**Important**: The non-loop variants (`while`, `until`) do NOT update captured variables between iterations. Use the `-loop` variants for any loop that needs to track state.

## Math (math.froth)

Mathematical utilities.

| Name | Stack Effect | Description |
|------|--------------|-------------|
| `fact` | `( n -- n! )` | Compute factorial of n |
| `fib` | `( a b n -- vals... )` | Generate Fibonacci sequence of length n |

The `fact` function computes the factorial of n. Returns 1 for n <= 1.

```
0 fact!            ; 1
5 fact!            ; 120
10 fact!           ; 3628800
```

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

