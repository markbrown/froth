# Froth

Froth is a stack-based programming language with lexical scoping.

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
```

### REPL

The REPL maintains state across lines: stack and variable bindings persist. Press Ctrl-D to exit.

### Runtime Errors

| Error | Cause |
|-------|-------|
| `stack underflow in 'op'` | Operation `op` needed more values than available |
| `type error: expected T, got U` | Operation expected type T but got type U |
| `undefined name: N` | Name N is not bound and is not an operator |
| `index out of bounds: I (array size: S)` | Array index I is not in range 0..S-1 |

## Language Comparison

Froth combines Forth's stack-based execution with Scheme's lexical scoping and first-class closures.

| | Forth | Scheme | Froth |
|-|-------|--------|-------|
| Paradigm | Stack-based, imperative | Functional, expression-based | Stack-based, functional |
| Syntax | `1 2 +` | `(+ 1 2)` | `1 2 +` |
| Scoping | Dynamic | Lexical | Lexical |
| Closures | No | Yes | Yes, introspectable |
| Metaprogramming | Compile-time macros | Homoiconic (code as lists) | Homoiconic (code as terms) |

## Documentation

- [FROTH.md](FROTH.md) - Language reference
- [FROTH_LIBRARY.md](FROTH_LIBRARY.md) - Standard library reference
- [FROTH_GUIDE.md](FROTH_GUIDE.md) - Coding guide and patterns
- [FROTH_COMPILER.md](FROTH_COMPILER.md) - Compiler infrastructure reference
- [FROTH_BYTECODE.md](FROTH_BYTECODE.md) - Bytecode compiler design
