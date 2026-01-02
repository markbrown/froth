# Froth Coding Guide

Common patterns and pitfalls when writing Froth code.

## Function Calls Require `!`

Standard library functions like `nl`, `println`, and `writeln` are closures, not operators. They must be called with `!`:

```
; Wrong
"hello" println

; Correct
"hello" println!
```

Without `!`, the closure is just pushed onto the stack instead of being executed.

## Boolean Convention: 0 is True

Froth uses 0 for true and non-zero for false. This matches the Unix convention where 0 indicates success.

```
3 3 =              ; 0 (equal, true)
3 4 =              ; 1 (not equal, false)
5 3 >              ; 0 (5 > 3, true)
```

## Avoid Redundant `0 =`

Since 0 already means true, testing `0 =` is usually unnecessary:

```
; Redundant
x y = 0 = { "equal" } { "not equal" } ?!

; Better
x y = { "not equal" } { "equal" } ?!
```

Note the branches are swapped because `=` returns 0 for equal (the "true" case).

## Conditional Syntax

The conditional operator `?` takes three values: condition, then-branch, else-branch. Use `?!` to immediately execute the chosen branch:

```
condition { then-branch } { else-branch } ?!
```

The then-branch executes when condition is 0 (true):

```
x 0 > { "positive" } { "not positive" } ?! println!
```

For simple values without execution, use `?` alone:

```
flag "yes" "no" ?    ; pushes "yes" if flag is 0, else "no"
```

## Binder Order is LIFO

Binders pop values in reverse order (last-in, first-out):

```
1 2 3 /a /b /c    ; c=3, b=2, a=1
```

When defining a function, list parameters in the order they're passed:

```
{ /a /b a b - } /subtract
5 3 subtract!     ; computes 5 - 3 = 2, not 3 - 5
```

## The `?` Operator Consumes the Condition

The conditional operator `?` pops and consumes the condition from the stack. After `?` executes, only the result remains:

```
; Stack: value flag
; After ?: just value (if flag was 0) or just alternate

42 0 { } { 99 } ?!    ; leaves 42 (condition 0 = true, keeps value)
42 1 { } { 99 } ?!    ; leaves 99 (condition 1 = false, runs else which pushes 99)
```

This matters when branching based on a flag while preserving other values:

```
; Wrong - tries to drop when nothing to drop
result 1 { drop! 1 } { 0 } ?!

; Right - else branch just returns the flag
result 1 { 0 } { 1 } ?!
```

## Bindings Are Lexically Scoped

Bindings created with `/name` do not escape their closure. Values must be returned via the stack:

```
; Wrong - x is not visible outside
{ 42 /x } !
x println!           ; error: x not bound

; Correct - return value on stack
{ 42 } ! /x
x println!           ; prints 42

; Or just leave it on the stack
{ 42 } !
println!             ; prints 42
```

Closures communicate results by leaving values on the stack:

```
{ /a /b a b + } /add
3 4 add! println!    ; prints 7
```

## Common Patterns

### Early Return via Conditional

```
{ /n
  n 0 < { "negative" } {
    n 0 = { "zero" } { "positive" } ?!
  } ?!
} /classify
```

### Iteration with foldl

```
[1 2 3 4 5] { println! } foldl!    ; prints each element
[1 2 3 4 5] { + } reduce!          ; sums to 15
```

### Building Results in a Generator

```
[ [1 2 3] { 2 * } foldl! ]         ; produces [2 4 6]
```

### Checking Types Before Operations

```
{ /x
  x isInt { x 1 + } { 0 } ?!
} /safe-increment
```

### Return Value with Success/Failure Flag

Many functions return a value plus a flag (0 = success, 1 = failure):

```
; alist-get returns (value 0) on success, (1) on failure
alist key alist-get!
{ /value
  ; use value
} {
  ; handle not found
} ?!
```

### Carrying State Through foldl

Use the stack to carry accumulator state through iterations:

```
; Sum with running state
0                           ; initial accumulator
[1 2 3 4 5] { + } foldl!    ; adds each element
; leaves 15

; Track multiple values: (result found-flag)
. 1                         ; nil result, not-found flag
array {
  /elem /found /result
  found {
    result 0                ; already found, pass through
  } {
    ; check elem, update result and flag
    elem matches { elem 0 } { result 1 } ?!
  } ?!
} foldl!
```

### Using def-fn! for Minimal Environments

When defining library functions, use `def-fn!` to capture only needed dependencies:

```
; List all stdlib functions used in the closure
['foldl 'map 'concat] {
  ; function body using foldl!, map!, concat!
} def-fn! /my-function
```

This prevents environment bloat and is required for compilation. Note: operators like `+`, `=`, `?` don't need to be listed—only closures from the standard library.
