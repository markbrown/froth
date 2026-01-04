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

## Quote `'` is a Prefix

The quote token `'` comes before the term it quotes, not after. Names like `code'` or `x'` are not valid identifiers—they parse as two tokens:

```
'x       ; quote + identifier = one quoted term
x'       ; identifier x followed by quote (two separate tokens!)

code'    ; parses as: code '
new-code ; valid identifier with hyphen
```

If you want a "prime" variable name, use a suffix like `2` or a prefix like `new-`:

```
; Wrong - x' is two tokens
/x x 1 + /x'

; Correct alternatives
/x x 1 + /x2
/x x 1 + /new-x
```

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

; Better - just remove the 0 =
x y = { "equal" } { "not equal" } ?!
```

Both work because `=` returns 0 for equal, and `?` executes the then-branch when condition is 0.

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

When defining a function, the first binder gets the top of stack (last argument pushed):

```
{ /a /b a b - } /subtract
5 3 subtract!     ; a=3, b=5, computes 3 - 5 = -2

{ /b /a a b - } /subtract2
5 3 subtract2!    ; a=5, b=3, computes 5 - 3 = 2
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

## Avoid Arrays as Accumulators

Arrays in Froth are immutable—every modification creates a copy. Never use arrays as accumulators:

```
; WRONG: O(n²) - each concat copies the entire array
result [item] concat! /result
```

Instead, use lists or maps which support O(1) updates:

```
; RIGHT: O(1) - cons just creates a new cell
result item , /result
```

Convert to an array at the end if needed: `[ list { } lfoldl! ]`

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

### Recursive Functions

Closures capture the environment at definition time. This means a function can't directly call itself because it isn't bound yet when the closure is created:

```
; This doesn't work - factorial isn't bound when closure is created
{ /n
  n 0 = { 1 } { n 1 - factorial! n * } ?!
} /factorial
5 factorial!             ; error: undefined name: factorial
```

Use Y-combinator style: pass the function to itself as an argument:

```
; Define with /self as FIRST binder (it will be popped last)
{ /self /n
  n 0 = { 1 } {
    n 1 - self self! n *
  } ?!
} /fact-impl

; Wrapper: push impl, args, impl again, then call
{ /n fact-impl n fact-impl! } /factorial
5 factorial!             ; 120
```

The pattern is: `{ /self /args body }` with recursive calls `args self self!`. The first `self` becomes `/self` in the next call (passed through the stack), and `self!` makes the call.

For tree traversal with multiple recursive calls:

```
{ /self /tree
  tree isNil { 0 } {
    tree 2 @ self self!    ; left subtree
    tree 3 @ self self!    ; right subtree
    + 1 +                   ; count this node
  } ?!
} /count-impl

{ /tree count-impl tree count-impl! } /tree-count
```

### Inspecting Closures

Use `open` to decompose a closure into its environment and body:

```
{ 1 2 + } /f
f open /body /env
env #              ; number of bindings captured
body               ; the quoted function body
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

For recursive functions with `def-fn!`, include the impl function in the dependencies of the wrapper:

```
{ /self /n
  n 0 = { 1 } { n 1 - self self! n * } ?!
} /fact-impl

['fact-impl] { /n fact-impl n fact-impl! } def-fn! /factorial
```
