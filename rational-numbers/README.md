# Rational Numbers

A parser and library for exact rational number literals in Standard ML —
the foundation the [rational-pl0-interpreter](../rational-pl0-interpreter)
project builds on.

- **`bigint.sml`** (`BIGINT.sig`) — arbitrary-precision integers over
  strings (add/subtract/multiply/divide/compare/gcd), so precision isn't
  limited to machine word size.
- **`rat.sml`** (`rat.sig`) — a `Rational` functor over any `BIGINT`
  implementation: exact rational arithmetic, plus conversion to/from
  decimal notation (including detecting and printing repeating decimals,
  e.g. `1/3` as `0.(3)`).

`GRAMMAR.md` is the formal grammar for rational number literals (integer,
fraction, decimal, and repeating-decimal forms) this parser accepts.

## `archive/`

`rat-earlier-draft.sml` — an earlier version of the `Rational` functor.
