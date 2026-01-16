# Froth! Bytecode Compiler Design

## Overview

A bytecode compiler for Froth! to improve execution performance by eliminating tree-walking overhead.

See FROTH_COMPILER.md for detailed pass documentation, node structure, and API reference.

## Key Behaviors

**Bottom-up compilation**: When `compile-func` encounters a nested function term, it recursively compiles the nested function first. The resulting func-node (with `'func-addr` already set) replaces the original term in the outer function's body.

**Passes don't recurse into nested functions**: The analysis passes (boundness, liveness, slots) and codegen read data from already-complete nested func-nodes rather than recursing into them. They do recurse into generators, which share the outer frame.

**Codegen reads `'func-addr` from nested nodes**: For nested function terms, codegen emits code to build the context array and create a closure using the pre-computed `node 'func-addr @`. It does not recursively compile.

## Limitations

Compilation can fail for functions using dynamic constructs (`env`, `import`, `applyOperator`) that prevent static analysis.

## Image Serialization

Save evaluator state to a binary file for later restoration. The image contains bytecode, the constant pool, the string table, and metadata allowing the environment to be reconstructed.

### Design Overview

Rather than a custom binary format for values, we compile values into reconstruction bytecode that rebuilds them when executed. This reuses the existing VM as the deserializer.

**Saving:**

1. Generate reconstruction bytecode that rebuilds the constant pool and environment
2. Write header (magic, version, bytecode-top, string-count, entry-addr)
3. Write string table (all interned strings)
4. Write bytecode (original + reconstruction code)

**Loading (via command line):**

1. Read and validate header
2. Populate string table map (restores string ↔ ID mappings for `intern`)
3. Load bytecode into store
4. Execute reconstruction code from entry-addr (rebuilds constant pool via `ref`)
5. Enter REPL with reconstructed environment

### Sharing Preservation

Shared substructures are preserved via deep-ref: when `ref` is called on a structured value, it recursively pools all subterms first. This ensures reconstruction code can use `deref` to retrieve shared subterms.

The `ref` operator returns -1 for primitives (int, string, nil) and closurevals (which must be compiled to bytecodevals first). Only structured values (array, map, cons, bytecodeval) get pool indices.

### Reify Function

Generates reconstruction bytecode for a value.

**Stack effect:** `( addr start-idx -- next-addr )`

**Process:**

1. Loop: `idx deref` → if nil, stop; otherwise emit reconstruction code for value, increment idx, repeat
2. For each subterm, call `ref` to get index: if >= 0 emit `deref`, if -1 emit inline
3. After reconstructing each value, emit `ref drop` to store it in the pool

The `deref` operator returns nil for out-of-bounds indices, providing loop termination.

**Per-type emission:**

| Type | Emit |
|------|------|
| intval | `push-int N` |
| stringval | `push-string ID` |
| nilval | `op .` |
| consval | emit tail, emit head, `op ,` |
| arrayval | `start-array`, emit elements, `end-array` |
| mapval | `op $`, for each: emit value, `push-int key`, `op idToIdent`, `op :` |
| bytecodeval | `start-array`, emit context, `end-array`, `push-int addr`, `op close` |
| termval | see below |

**Term emission:**

| Term | Emit |
|------|------|
| identifier(id) | `push-int id`, `op idToIdent` |
| binder(id) | `push-int id`, `op idToBinder` |
| apply_term | `push-quoted-apply` |
| function(terms) | emit term array, `op mkFunc` |
| generator(terms) | emit term array, `op mkGen` |
| quoted(term) | emit inner, `op wrap` |
| value(val) | emit value, `op wrap` |
