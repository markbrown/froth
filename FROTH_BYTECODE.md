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

Shared substructures must be preserved across save/load. We achieve this by modifying `ref` to recursively pool subterms before pooling a value (bottom-up). Reconstruction code uses `deref` to retrieve already-built subterms.

Example: `[. 2, . 2, 1,]` (array with two cons lists sharing tail `. 2,`)

**Deep-ref process:**

1. ref `. 2,` → index 0
2. ref `. 2, 1,` → index 1 (tail already at index 0)
3. ref array → index 2

**Reconstruction code:**

```
. 2, ref drop           ; index 0
0 deref 1, ref drop     ; index 1 (reuses tail via deref)
[0 deref 1 deref] ref drop  ; index 2
```

Only structured values (cons, array, map, closure) need pool entries. Primitives (int, string, nil) are emitted inline.

### Stage 1: Deep Ref

Modify `ref` operator to recursively pool structured subterms before pooling the value itself.

**Current behavior:** Store value directly, return index (with deduplication).

**New behavior (algorithm):**

1. If primitive (int, string, nil) or closureval: return -1 (not pooled)
2. Search hash table for existing index; return if found
3. Recurse according to compound type (ignore results, just ensures subterms are pooled):
   - arrayval: ref each element
   - mapval: ref each value
   - consval: ref head and tail
   - bytecodeval: ref the context array
   - termval: don't recurse (avoids complexity; terms are syntax)
4. Add value to hash table, return index

**Why closureval returns -1:** For image serialization, all closurevals must be compiled to bytecodevals first. Returning -1 signals "not suitable for pooling". This is a breaking change for code that refs closurevals directly.

**Testing:**

```
; Create shared substructure
. 2, /tail
tail 1, /list1
tail 3, /list2
[list1 list2] ref drop

; Verify tail was pooled with consistent index
tail ref /idx1
tail ref /idx2
idx1 idx2 =  ; 0 (same index, sharing preserved)

; Verify deref retrieves correct value
idx1 deref tail =  ; 0 (values equal)

; Primitives return -1
42 ref -1 =  ; 0
. ref -1 =   ; 0

; Closurevals return -1 (must compile to bytecodeval first)
{ 1 } ref -1 =  ; 0
```

This change is self-contained: existing code that refs structured values gets the same index, with subterms also pooled as a side effect. Code that refs primitives or closurevals now gets -1 instead of a pool index.
