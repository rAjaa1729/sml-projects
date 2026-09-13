# While-VM

A stack-based virtual machine (VMC: Value/Memory/Command) for the same
While-style imperative language used in
[rational-pl0-interpreter](../rational-pl0-interpreter) — compiled and
executed via reduction rules over three stacks, instead of directly
tree-walking the AST.

## How it works

1. `compiler.sml` parses a program into an AST (declarations + commands).
2. The declaration list becomes a fixed symbol table (`Table`, mapping
   names to memory slots); the command list becomes the initial command
   stack.
3. `vmc.sml` repeatedly applies reduction rules to the (command stack,
   value stack, memory) state until the command stack is empty, a type
   error occurs (e.g. comparing incompatible types), or no rule applies.

`table.sml` (symbol table) is course-provided scaffolding; the VMC
machine and its reduction rules (`vmc.sml`) are the assignment itself.

## Run it

```sml
CM.make "while.cm";
Vmc.execute "testdata/test4.while";
```

## `testdata/`

Sample While programs: loops/nested loops (`test4.while`), branching
(`test2.while`, `test3.while`), and I/O (`test5.while`).
