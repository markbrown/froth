# Froth

Froth is a stack-based programming language with lexical scoping.

## Project Overview

Froth is implemented in Mercury, a pure logic/functional programming language. The interpreter includes a lexer, parser, and evaluator, with a REPL for interactive use.

```
froth/
├── bin/                  # Compiled binary
├── lib/                  # Standard library
│   ├── stdlib.froth      # Standard library (auto-loaded)
│   ├── io.froth          # I/O utilities (nl, println, writeln)
│   ├── array.froth       # Array utilities (fold, map, filter)
│   └── eval.froth        # Meta-interpreter
├── src/                  # Mercury source
│   ├── froth.m           # Main entry point, REPL
│   ├── lexer.m           # Tokenizer
│   ├── parser.m          # Parser
│   ├── eval.m            # Evaluator
│   ├── operators.m       # Operator definitions
│   └── types.m           # Type definitions
├── tests/                # Regression tests
│   ├── *.froth           # Test programs
│   ├── *.expected        # Expected outputs
│   └── lib/              # Test library files
├── FROTH.md              # Language reference
├── Makefile              # Build: make, make test, make clean
└── run_tests.sh          # Test runner script
```

## Usage

```
Usage: froth [OPTIONS] [FILE]

Options:
  -n, --no-stdlib    Don't auto-load standard library
  -q, --quiet        Suppress REPL banner
  -e, --exec CODE    Execute CODE
  -f, --file FILE    Execute FILE
  -i, --interactive  Start REPL after -e/-f
  -h, --help         Show this help

If FILE given without -f, treat as -f FILE.
If no -e/-f/FILE, start REPL.
Multiple -e/-f processed in order.
```

```bash
froth                           # Start interactive REPL (with stdlib)
froth program.froth             # Run a file
froth -e "1 2 + println!"       # Execute code string
froth -f lib.froth -e "go!"     # Load file, then execute code
froth -i program.froth          # Run file, then start REPL
froth -n -e "1 2 + print"       # Execute without stdlib
froth -q                        # Start REPL without banner
```

### Importing Files

Use the `import` operator to load additional Froth files:

```
"utils.froth" import     ; Load utils.froth relative to current file
```

The standard library (`lib/stdlib.froth`) is loaded automatically unless `-n` is given.

### REPL

The REPL (Read-Eval-Print Loop) maintains state across lines:

- The stack persists between inputs
- Variable bindings persist between inputs
- Press Ctrl-D to exit

### Saving and Restoring State

You can save the current environment and reload it later:

```
; In the REPL, save environment to a file:
env ["session" ".froth"] fwrite

; Later, restart with that environment:
; $ froth -e session.froth
```

## Tips

### Lexical Scoping

Functions capture the environment at definition time. When applied, they execute with their captured environment, and any environment changes are discarded after execution. The stack is shared.

```
1 /x { x x + } /f      ; f captures x=1
2 /x                   ; rebind x to 2
f ! print              ; prints 2 (1+1), not 4
```

### Inspecting Closures

Closures are represented as cons pairs of `(environment-map, quoted-function)`:

```
{ 1 2 + } /f
f fst # print          ; prints number of bindings in closure's environment
f snd print            ; prints: { 1 2 + }
```

### Shell Escaping

When piping Froth code through bash, characters like `!`, `$`, `*`, and `?` may be interpreted by the shell. Use a heredoc with a quoted delimiter to avoid this:

```bash
cat <<'EOF' | froth
5 3 > { "yes" } { "no" } ?!
EOF
```

Or use files or the REPL, which don't have this issue.

## Examples

### Hello World
```
"Hello, World!" print
```

### Arithmetic
```
1 2 + print            ; 3
10 3 - print           ; 7
4 5 * print            ; 20
```

### Variables
```
42 /answer
answer print           ; 42
```

### Functions
```
{ 2 + } /add-two
5 add-two! print       ; 7
```

The `!` can be written immediately after the function name, emphasizing the imperative nature of application ("do it!").

### Conditionals
```
; ? takes: condition, then-value, else-value
; 0 means true, non-zero means false
5 3 > 'yes 'no ? print ; yes (5 > 3)
```

### Arrays
```
[ 1 2 3 ] print        ; 123
[ 10 20 30 ] # print   ; 3 (length)
[ 10 20 30 ] 1 @ print ; 20 (index)
```

### Cons Lists
```
. print                ; . (nil)
. 1 , print            ; (1,.)
. 3 , 2 , 1 , /x       ; build list [1, 2, 3]
x fst print            ; 1
x snd fst print        ; 2
```

### Maps
```
$ print                       ; <map:0>
$ 42 'x : 10 'y : /m          ; create map with x=42, y=10
m 'x @ print                  ; 42
m keys # print                ; 2
```

### Recursion
```
; Factorial using Y-combinator style
{ /self /n
  n 0 = { 1 } { n n 1 - self self! * } ?!
} /fact

5 fact fact! print     ; 120
```

## Language Comparison

Froth belongs to the concatenative/stack-based language family, but combines ideas from multiple traditions:

**From Forth**: Stack-based execution, postfix notation, minimal syntax, concatenative style.

**From Scheme**: Lexical scoping, first-class closures, quoting mechanism, minimalist design philosophy.

| Feature | Forth | Scheme | Froth |
|---------|-------|--------|-------|
| Execution | Stack-based | Expression evaluation | Stack-based |
| Syntax | Postfix | Prefix `(+ 1 2)` | Postfix `1 2 +` |
| Scoping | Dynamic | Lexical | Lexical |
| Closures | No | Yes | Yes, inspectable as `(env, body)` |
| Quoting | Limited | `'x` | `'x` |
| Code as data | No | S-expressions | Quoted terms |

Froth could be described as "Forth with Scheme's scoping semantics" - it looks like Forth but thinks more like Scheme.

### Related Languages

- **Factor** - Modern concatenative language with quotations and lexical scope
- **Joy** - Pure functional concatenative language
- **PostScript** - Stack-based language for graphics
- **RPL** - HP calculator language with stack and quoting

## Documentation

See [FROTH.md](FROTH.md) for the complete language reference.
