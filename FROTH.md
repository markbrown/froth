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

### Comments and Whitespace

- Line comments: `;` to end of line
- Block comments: `( ... )` (nestable)
- Spaces, tabs, and newlines separate tokens but are otherwise ignored

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

| Type | Description |
|------|-------------|
| `int` | Integer |
| `string` | String |
| `array` | Array of values |
| `map` | Map from identifiers to values |
| `term` | Quoted (unevaluated) term |
| `nil` | Empty/null value |
| `cons` | Pair of two values (head, tail) |
| `closure` | Closure (environment + function body) |

## Evaluation

### Evaluation State

Evaluation maintains three pieces of state:

- **Stack**: A list of values (LIFO)
- **Environment**: A map from names to values
- **I/O**: Standard input/output

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
| `isCons` | `( a -- int )` | 0 if cons, else 1 |
| `isClosure` | `( a -- int )` | 0 if closure, else 1 |
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
| `print` | `( a -- )` | Pop and print a value (nil prints nothing, cons/arrays concatenate elements) |
| `write` | `( a -- )` | Print in executable (round-trippable) form |
| `fwrite` | `( value file -- )` | Write value to file in executable form |
| `import` | `( path -- )` | Load and evaluate a Froth file (path relative to current file) |
| `time` | `( -- int )` | Push current clock ticks |

### Metaprogramming

| Name | Stack Effect | Description |
|------|--------------|-------------|
| `env` | `( -- map )` | Push current environment as a map |
| `restore` | `( map -- )` | Replace current environment with the map |
| `close` | `( env body -- closure )` | Create a closure from environment and function body |
| `closureEnv` | `( closure -- env )` | Extract environment from a closure |
| `closureBody` | `( closure -- body )` | Extract function body from a closure |
| `unwrap` | `( 'x -- x )` | Extract inner from quoted value or quoted term |
| `intern` | `( a -- int )` | Get intern id from string, identifier, or binder |
| `idToString` | `( int -- string )` | Create string from intern id |
| `idToIdent` | `( int -- 'ident )` | Create quoted identifier from intern id |
| `idToBinder` | `( int -- 'binder )` | Create quoted binder from intern id |
| `isOperator` | `( 'ident -- int )` | 0 if identifier is an operator, else 1 |
| `arity` | `( 'ident -- int )` | Get arity (input count) of an operator |
| `stack` | `( ... -- array )` | Convert entire stack to an array |
| `peek` | `( addr -- int )` | Read value from bytecode store (0 if beyond size) |
| `poke` | `( value addr -- )` | Write value to bytecode store (extends if needed) |
| `applyOperator` | `( 'op -- ... )` | Apply a quoted operator |
