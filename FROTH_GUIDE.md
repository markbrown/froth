# Froth! Coding Guide

Common patterns and pitfalls when writing Froth! code.

## Function Calls Require `!`

Standard library functions are closures, not operators. They must be called with `!`:

```
"hello" println!
```

Without `!`, the closure is pushed onto the stack instead of being executed.

## Quote `'` is a Prefix

The quote token `'` comes before the term it quotes. Names like `x'` parse as two tokens (`x` followed by `'`). Use suffixes like `x2` or prefixes like `new-x` instead.

## Boolean Convention: 0 is True

Froth! uses 0 for true and non-zero for false (like Unix exit codes).

```
3 3 =              ; 0 (equal, true)
3 4 =              ; 1 (not equal, false)
```

Since 0 already means true, avoid redundant `0 =` tests:

```
x y = { "equal" } { "not equal" } ?!   ; not: x y = 0 = { ... } ...
```

## Conditional Syntax

The `?` operator takes condition, then-branch, else-branch. The then-branch executes when condition is 0:

```
x 0 > { "positive" } { "not positive" } ?!
```

## Binder Order is LIFO

Binders pop values in reverse order. The first binder gets the top of stack:

```
{ /a /b a b - } /subtract
5 3 subtract!     ; a=3, b=5, computes 3 - 5 = -2
```

## Bindings Are Lexically Scoped

Bindings created with `/name` do not escape their closure. Return values via the stack:

```
{ 42 } ! /x       ; correct: value returned on stack, then bound
{ 42 /x } ! x     ; wrong: x not visible outside closure
```

## Avoid Arrays as Accumulators

Arrays are immutable—every modification copies. Use cons lists for O(1) accumulation:

```
result item , /result              ; O(1) cons
[ result { } lfoldl! ]             ; convert to array at end
```

## Common Patterns

### Return Value with Success/Failure Flag

Many functions return `(value 0)` on success, `(1)` on failure:

```
alist key alist-get! { /value ... } { ; not found } ?!
```

### Recursive Functions

Closures capture the environment at definition time, so a function can't call itself directly. Pass the function to itself:

```
{ /self /n
  n 0 = { 1 } { n 1 - self self! n * } ?!
} /fact-impl

{ /n fact-impl n fact-impl! } /factorial
5 factorial!             ; 120
```

### Using def-fn! for Minimal Environments

Use `def-fn!` to capture only needed dependencies (required for compilation):

```
['foldl 'map] { ; body using foldl!, map! } def-fn! /my-function
```

Operators like `+`, `=`, `?` don't need to be listed—only closures.
