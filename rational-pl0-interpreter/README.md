# Rational-PL0 Interpreter

An interpreter for **Rational-PL0**, a small imperative language (variables,
procedures, `if`/`while`, `read`/`print`) extended with arbitrary-precision
integers and exact rational-number arithmetic as built-in types. Built in
Standard ML using ML-Lex and ML-Yacc.

## How it works

- **`rational_PL0.lex` / `.yacc`** — lexer and grammar, producing an AST
  (`datatypes.sml`) of programs, procedures, commands (`assign`, `call`,
  `if`, `while`, `read`, `print`) and expressions (integer ops, rational
  ops like `.+.`/`.*.`/`make_rat`/`fromDecimal`, booleans, comparisons).
- **`bigint.sml`** — arbitrary-precision integers implemented over strings
  (so integer arithmetic isn't limited to machine word size).
- **`rational.sml`** — exact rational numbers built on `BigInt`, with
  conversions to/from decimal notation.
- **`dict.sml`** — scoped symbol table for variables and procedures
  (supports nested procedure declarations, see `testdata/nested_procedures.pl0`).
- **`compiler.sml` / `rat_exe.sml`** — walks the AST and evaluates it,
  writing output to a file.

## Run it

Needs [SML/NJ](https://www.smlnj.org/).

```sml
CM.make "rational_PL0.cm";
```

This compiles the interpreter and, via the last line of `rat_exe.sml`,
immediately runs `testdata/factorial_power_bool.pl0` and writes its output
to `outfile.txt`. To run a different program, call `interpret(path,
outfile)` directly, e.g. `interpret ("testdata/power.pl0", "out.txt");`.

## `testdata/`

Sample programs exercising different features: `factorial.pl0`,
`power.pl0` (integer loops/procedures), `bool_conversion.pl0`,
`nested_procedures.pl0` (scoping), `rational_expressions.rat` (rational
arithmetic), and `factorial_power_bool.pl0` with its expected output.

## `bool-calculator-warmup/`

An earlier, simpler exercise with the same lexer/parser toolchain: a
basic boolean-expression calculator, done before tackling the full
language above.

## `archive/`

`rat_exe-stdout-variant.sml` — an earlier driver that printed directly to
stdout instead of a file. `ratpackage-bigint-attempt/` — a superseded
first attempt at the bigint implementation, not used by the final build.

## `docs/`

The original assignment brief.
