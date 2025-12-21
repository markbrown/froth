# Froth Language Reference

Froth is a stack-based programming language with lexical scoping.

## Command Line

```
Usage: froth [options] [filename]

Options:
  -e FILE    Execute FILE then start REPL
  -f FILE    Run FILE
  -h, --help Show this help

With no arguments, starts an interactive REPL.
```

### Examples

```bash
froth                    # Start interactive REPL
froth program.froth      # Run a file
froth -f program.froth   # Run a file (explicit)
froth -e prelude.froth   # Load file then start REPL
froth --help             # Show help
```

### Saving and Restoring State

You can save the current environment and reload it later:

```
; In the REPL, save environment to a file:
env ["session" ".froth"] fwrite

; Later, restart with that environment:
; $ froth -e session.froth
```

### REPL

The REPL (Read-Eval-Print Loop) maintains state across lines:

- The stack persists between inputs
- Variable bindings persist between inputs
- Press Ctrl-D to exit

## Lexical Syntax

### Tokens

| Token | Pattern | Description |
|-------|---------|-------------|
| `name` | `[a-zA-Z+=,.<>?@#:$-][a-zA-Z0-9+=,.<>?@#:$-]*` | Identifier |
| `slash_name` | `/` followed by a name | Binder (variable definition) |
| `number` | `[-+][0-9]+` or `[0-9]+` | Integer literal (`+`/`-` followed by digit) |
| `string` | `"..."` | String literal (supports escape sequences) |
| `'` | | Quote (prevents evaluation) |
| `!` | | Apply (execute a closure) |
| `{` `}` | | Function delimiters |
| `[` `]` | | Generator delimiters |

**Note:** If a token starts with `+` or `-` followed by a digit, it is parsed as a number. Otherwise, `+` and `-` are treated as name characters. This means `1+2` parses as two tokens: `1` and `+2` (both numbers), while `a+b` is a single name token.

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
identifier  ::= name
binder      ::= slash_name
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

## Evaluation Rules

Evaluation maintains three pieces of state:

- **Stack**: A list of values (LIFO)
- **Environment**: A map from names to values
- **I/O**: Standard input/output

### Term Evaluation

| Term | Rule |
|------|------|
| `identifier` | If builtin, execute it. Otherwise, look up name in environment and push its value. |
| `binder` (`/name`) | Pop a value and bind it to `name` in the environment. |
| `function` (`{ ... }`) | Capture the current environment and push a closure. |
| `generator` (`[ ... ]`) | Evaluate terms with an empty stack, collect results into an array, push the array. |
| `quoted` (`'term`) | Push the term as a value without evaluating it. |
| `apply` (`!`) | Pop a closure and execute it. Stack is shared; environment changes are discarded. |
| `literal` | Push the value onto the stack. |

### Lexical Scoping

Functions capture the environment at definition time. When applied, they execute with their captured environment, and any environment changes are discarded after execution. The stack is shared.

```
1 /x { x x + } /f      ; f captures x=1
2 /x                   ; rebind x to 2
f ! print              ; prints 2 (1+1), not 4
```

### Closure Representation

Closures are represented as cons pairs of `(environment-map, quoted-function)`. This means:

- `{ terms }` is equivalent to `env '{ terms } ,`
- Closures can be inspected with `fst` (get environment) and `snd` (get quoted function body)
- Closures can be compared for equality with `=`

```
{ 1 2 + } /f
f fst # print          ; prints number of bindings in closure's environment
f snd print            ; prints: { 1 2 + }
```

## Builtins

| Name | Stack Effect | Description |
|------|--------------|-------------|
| `+` | `( a b -- a+b )` | Pop two integers, push their sum |
| `=` | `( a b -- int )` | Push 0 if equal, 1 if not equal |
| `?` | `( cond then else -- result )` | If cond is 0, push then; otherwise push else |
| `@` | `( container key -- val )` | Get element: array requires int index, map requires quoted identifier (`'name`) |
| `#` | `( container -- int )` | Get length of array or map |
| `.` | `( -- nil )` | Push nil onto the stack |
| `,` | `( tail head -- cons )` | Create a cons cell with head and tail |
| `fst` | `( cons -- head )` | Get the first element (head) of a cons |
| `snd` | `( cons -- tail )` | Get the second element (tail) of a cons |
| `$` | `( -- map )` | Push an empty map |
| `:` | `( map val 'key -- map )` | Store value in map under key, returning new map |
| `keys` | `( map -- array )` | Get map keys as an array of quoted identifiers |
| `env` | `( -- map )` | Push current environment as a map |
| `print` | `( a -- )` | Pop and print a value |
| `write` | `( a -- )` | Pop and print a value in executable (round-trippable) form |
| `fwrite` | `( value file -- )` | Write value to file in executable form |
| `dump` | `( -- )` | Print the entire stack (for debugging) |

## Examples

### Hello World
```
"Hello, World!" print  ; prints Hello, World!
```

### Arithmetic
```
1 2 + print            ; prints 3
```

### Variables
```
42 /answer
answer print           ; prints 42
```

### Functions
```
{ 2 + } /add-two
5 add-two ! print      ; prints 7
```

### Arrays
```
[ 1 2 3 ] print        ; prints 123
[ 10 20 30 ] # print   ; prints 3
[ 10 20 30 ] 1 @ print ; prints 20
```

### Nested Functions
```
{ { 1 } } ! ! print           ; prints 1
```

### Cons Lists
```
. print                       ; prints .
. 1 , print                   ; prints (1,.)
. 3 , 2 , 1 , /x              ; build list [1, 2, 3]
x fst print                   ; prints 1
x snd fst print               ; prints 2
```

### Maps
```
$ print                       ; prints <map:0>
$ 42 'x : 10 'y : /m          ; create map with x=42, y=10
m # print                     ; prints 2
m 'x @ print                  ; prints 42
m keys # print                ; prints 2

; maps can be written and read back
m write                       ; prints: $ 10 'y : 42 'x :
```

### Quoting
```
'foo print             ; prints: foo
'{ 1 2 + } print       ; prints: { 1 2 + }
''foo print            ; prints: 'foo
```

### Environment
```
42 /x "hello" /y
env write              ; prints: $ 42 'x : "hello" 'y :
env # print            ; prints: 2
env 'x @ print         ; prints: 42
```

## Runtime Errors

| Error | Cause |
|-------|-------|
| `stack underflow in 'op'` | Operation `op` needed more values than available |
| `type error: expected T, got U` | Operation expected type T but got type U |
| `undefined name: N` | Name N is not a builtin and not in the environment |
| `index out of bounds: I (array size: S)` | Array index I is not in range 0..S-1 |
