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
