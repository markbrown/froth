# Froth Language Reference

## Lexical Syntax

### Tokens

| Token | Pattern | Description |
|-------|---------|-------------|
| `text_name` | `[a-zA-Z][a-zA-Z0-9-]*` | Text identifier |
| `graphical_name` | `[+-*=.,<>?@#:$]+` | Graphical identifier |
| `slash_name` | `/` followed by a name | Binder |
| `number` | `[0-9]+` or `[+-][0-9]+` | Integer literal |
| `string` | `"..."` | String literal |
| `'` | | Quote |
| `!` | | Apply |
| `{` `}` | | Function delimiters |
| `[` `]` | | Generator delimiters |

### Names

Identifiers are either *text* or *graphical*. Text names start with a letter and contain letters, digits, and hyphens (e.g., `foo`, `add-two`, `x1`). Graphical names consist of symbols (e.g., `+`, `->`, `<=>`). These two categories cannot be mixed: `foo+bar` parses as three tokens: `foo`, `+`, `bar`.

### Numbers

If a token starts with `+` or `-` followed by a digit, it is parsed as a number. This means `1+2` parses as two numbers: `1` and `+2`. The correct way to add is `1 2 +`.

### Binders

Both text and graphical names can be bound with `/`. Writing `/foo` binds a text name, and `/+` binds the `+` operator.

### Escape Sequences

String literals support the following escape sequences:

| Escape | Character |
|--------|-----------|
| `\\` | Backslash |
| `\"` | Double quote |
| `\n` | Newline |
| `\t` | Tab |
| `\r` | Carriage return |

### Comments

- Line comments: `;` to end of line
- Block comments: `( ... )` (nestable)

### Whitespace

Spaces, tabs, and newlines separate tokens but are otherwise ignored.

## Grammar

```
program     ::= term*
term        ::= identifier | binder | function | generator | quoted | apply | literal
identifier  ::= text_name | graphical_name
binder      ::= '/' (text_name | graphical_name)
function    ::= '{' term* '}'
generator   ::= '[' term* ']'
quoted      ::= "'" term
apply       ::= '!'
literal     ::= number | string
```

## Values

### Value Types

| Type | Description |
|------|-------------|
| `int` | Integer |
| `string` | String |
| `array` | Array of values |
| `map` | Map from identifiers to values |
| `term` | Quoted (unevaluated) term |
| `nil` | Empty/null value |
| `cons` | Pair of two values (head, tail) |

### Closure Representation

Closures are represented as cons pairs of `(environment-map, quoted-function)`. This means:

- `{ terms }` is equivalent to `'{ terms } env ,`
- Closures can be inspected with `fst` (get environment) and `snd` (get quoted function body)
- Closures can be compared for equality with `=`

## Virtual Machine

### Evaluation State

Evaluation maintains three pieces of state:

- **Stack**: A list of values (LIFO)
- **Environment**: A map from names to values
- **I/O**: Standard input/output

### Runtime Errors

| Error | Cause |
|-------|-------|
| `stack underflow in 'op'` | Operation `op` needed more values than available |
| `type error: expected T, got U` | Operation expected type T but got type U |
| `undefined name: N` | Name N is not an operator and not in the environment |
| `index out of bounds: I (array size: S)` | Array index I is not in range 0..S-1 |

## Evaluation

### Term Evaluation Rules

| Term | Rule |
|------|------|
| `identifier` | Look up name in environment; if found, push its value. Otherwise, if operator, execute it. Environment shadows operators. |
| `binder` (`/name`) | Pop a value and bind it to `name` in the environment. |
| `function` (`{ ... }`) | Capture the current environment and push a closure. |
| `generator` (`[ ... ]`) | Evaluate terms with an empty stack, collect results into an array, push the array. |
| `quoted` (`'term`) | Push the term as a value without evaluating it. |
| `apply` (`!`) | Pop a closure or quoted operator and execute it. Stack is shared; environment changes are discarded. |
| `literal` | Push the value onto the stack. |

## Operators

### Arithmetic

| Name | Stack Effect | Description |
|------|--------------|-------------|
| `+` | `( a b -- a+b )` | Add two integers |
| `-` | `( a b -- a-b )` | Subtract: `a b -` computes `a - b` |
| `*` | `( a b -- a*b )` | Multiply two integers |

### Comparison

| Name | Stack Effect | Description |
|------|--------------|-------------|
| `=` | `( a b -- int )` | Equal: 0 if equal, 1 if not equal |
| `>` | `( a b -- int )` | Greater than: 0 if `a > b`, else 1 |
| `<` | `( a b -- int )` | Less than: 0 if `a < b`, else 1 |
| `>=` | `( a b -- int )` | Greater or equal: 0 if `a >= b`, else 1 |
| `<=` | `( a b -- int )` | Less or equal: 0 if `a <= b`, else 1 |

### Control Flow

| Name | Stack Effect | Description |
|------|--------------|-------------|
| `?` | `( cond then else -- result )` | If cond is 0, push then; otherwise push else |

### Data Structures

| Name | Stack Effect | Description |
|------|--------------|-------------|
| `.` | `( -- nil )` | Push nil onto the stack |
| `,` | `( tail head -- cons )` | Create a cons cell |
| `fst` | `( cons -- head )` | Get the head of a cons cell |
| `snd` | `( cons -- tail )` | Get the tail of a cons cell |
| `$` | `( -- map )` | Push an empty map |
| `:` | `( map val 'key -- map )` | Store value in map under key |
| `@` | `( container key -- val )` | Get element by index (array, function, generator) or key (map) |
| `#` | `( container -- int )` | Get length of array, map, or quoted function/generator |
| `keys` | `( map -- array )` | Get map keys as array of quoted identifiers |
| `in` | `( map 'key -- int )` | Test if key exists in map: 0 if yes, 1 if no |
| `delete` | `( map 'key -- map )` | Remove key from map, return new map |

### Type Testing

| Name | Stack Effect | Description |
|------|--------------|-------------|
| `isInt` | `( a -- int )` | 0 if int, else 1 |
| `isString` | `( a -- int )` | 0 if string, else 1 |
| `isArray` | `( a -- int )` | 0 if array, else 1 |
| `isMap` | `( a -- int )` | 0 if map, else 1 |
| `isNil` | `( a -- int )` | 0 if nil, else 1 |
| `isCons` | `( a -- int )` | 0 if cons (including closures), else 1 |
| `isIdent` | `( a -- int )` | 0 if quoted identifier, else 1 |
| `isBinder` | `( a -- int )` | 0 if quoted binder, else 1 |
| `isFunc` | `( a -- int )` | 0 if quoted function, else 1 |
| `isGen` | `( a -- int )` | 0 if quoted generator, else 1 |
| `isQuote` | `( a -- int )` | 0 if quoted quote, else 1 |
| `isApply` | `( a -- int )` | 0 if quoted apply, else 1 |
| `isValue` | `( a -- int )` | 0 if quoted value term, else 1 |

### I/O

| Name | Stack Effect | Description |
|------|--------------|-------------|
| `print` | `( a -- )` | Pop and print a value |
| `write` | `( a -- )` | Print in executable (round-trippable) form |
| `fwrite` | `( value file -- )` | Write value to file in executable form |
| `import` | `( path -- )` | Load and evaluate a Froth file |

The `import` operator loads a file relative to the current file's directory. Definitions from imported files are added to the current environment.

### Metaprogramming

| Name | Stack Effect | Description |
|------|--------------|-------------|
| `env` | `( -- map )` | Push current environment as a map |
| `unwrap` | `( 'x -- x )` | Extract inner from quoted value or quoted term |
| `intern` | `( a -- int )` | Get intern id from string, identifier, or binder |
| `idToString` | `( int -- string )` | Create string from intern id |
| `idToIdent` | `( int -- 'ident )` | Create quoted identifier from intern id |
| `idToBinder` | `( int -- 'binder )` | Create quoted binder from intern id |
| `isOperator` | `( 'ident -- int )` | 0 if identifier is an operator, else 1 |
| `arity` | `( 'ident -- int )` | Get arity (input count) of an operator |
| `stack` | `( ... -- array )` | Convert entire stack to an array |

## Standard Library

The standard library (`lib/stdlib.froth`) loads automatically unless `-n` is given. It imports the following modules:

### I/O Utilities (io.froth)

| Name | Stack Effect | Description |
|------|--------------|-------------|
| `nl` | `( -- )` | Print a newline |
| `println` | `( a -- )` | Print a value followed by a newline |
| `writeln` | `( a -- )` | Write in executable form followed by a newline |

### Array (array.froth)

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

### List (list.froth)

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

### Eval (eval.froth)

A meta-interpreter that evaluates Froth closures.

| Name | Stack Effect | Description |
|------|--------------|-------------|
| `eval` | `( stack closure -- result-stack 0 \| error-msg 1 )` | Evaluate closure with initial stack |

The stack is represented as a cons list (nil-terminated). Push with `, `:

```
.                  ; empty stack (nil)
. 3 ,              ; stack with just 3
. 3 , 2 , 1 ,      ; stack [1, 2, 3] (1 on top)
```

Returns two values: `result-stack` and `0` on success, or `error-message` and `1` on error.

```
. { 3 4 + } eval!              ; returns (. 7 ,) 0
. { 10 /x x 1 + } eval!        ; returns (. 11 ,) 0
. 10 , { 5 + } eval!           ; returns (. 15 ,) 0
```

Supported: all operators (dispatched by arity), variable binding, function literals, function application, quoted terms.

Not yet supported: generators (`[ ]`), I/O operators, `env`, `stack`.
