# MDT: Markdown-ish to HTML

A translator from a lightweight Markdown-like format ("MDT") to HTML,
written in Standard ML as a hand-written recursive-descent scanner (no
lexer/parser generator).

## Supported syntax

Bold (`**`), italics (`*`), underline, direct and indirect hyperlinks,
block quotes, code blocks (indented 8+ spaces), unordered lists (nesting
by indentation), and tables (delimited by `<<`/`>>`). See
`docs/assignment-brief.pdf` for the exact spec and `testdata/mdtab.mdt`
for a real example.

## Run it

```sml
use "mdt2html.sml";
```

Reads `inputfile.txt` and writes HTML to `Answer.html` (both hardcoded —
copy `testdata/inputfile.txt` into the working directory to try it, and
compare against `testdata/expected.html`).

## `archive/`

Earlier drafts of the same converter (`mdt2html-early-draft.sml`,
`mdt2html-later-draft.sml`, `resume.sml`, `addlist.sml`), kept to show
the progression toward the final version above.
