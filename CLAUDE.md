# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Is

**treason** is a research language exploring what it takes to build a Lisp with hygienic macros *and* a great IDE experience. It's a tiny Lisp with a full Language Server Protocol (LSP) implementation, written in Racket. The language supports `let`, `define`, `begin`, `block`, `let-syntax`, `define-syntax`, and `syntax-rules` macros.

The core insight: IDE services must be **fault-tolerant**. The expander never gives up — it continues expanding past syntax errors, collects all of them, and still provides accurate goto-definition, find-references, and autocomplete on the well-formed parts.

Programs run as `#lang treason` modules: treason's own reader and expander process the file, every error in it is reported at once, and a program with no errors is compiled to Racket and run. The language itself is still small — there are no functions or application yet — so a running program computes with definitions, `let`, `block`, and macros.

## Commands

```bash
# Run all tests
raco test -p treason

# Run tests for a specific file
raco test lsp-tests.rkt
raco test reader-tests.rkt

# Run the language server (reads JSON-RPC from stdin)
racket server.rkt

# Run a treason program (a file whose first line is `#lang treason`)
racket program.rkt

# Install the package locally
raco pkg install --auto
```

## Architecture

The pipeline is: **source text → reader → stx → expander → ExpanderState**, which feeds two consumers: the **language server** (LSP operations over the cached result) and the **`#lang treason` compiler** (reports every error, or compiles the expanded program to Racket).

### Key Design Properties

- **Fault-tolerant expansion**: the expander continues after errors (unbound variables, bad syntax) and emits `stx-error` nodes. LSP features work even in incomplete or broken programs.
- **Hygienic macros**: scope graphs with marks ensure macro-introduced bindings don't capture surface identifiers and vice versa.
- **Pattern variable LSP support**: goto-definition, find-references, and autocomplete work on `syntax-rules` pattern variables in templates, even if the macro is never called.
- **Cursor-driven autocomplete**: autocomplete inserts a synthetic cursor node and re-expands to find names in scope at that position.

### Modules

- **`server.rkt`** — JSON-RPC LSP server. Implemented as a class wrapped by a thin JSON-RPC conversion layer. When an IDE user opens or edits a file, the file is parsed and expanded, and all static information (like bindings and resolutions) from expansion is cached. These cached results are used for LSP operations like goto-definition. LSP helper functions (`goto-definition`, `find-references`, `autocomplete`) are plain functions that take an `ExpanderResult` and a cursor location.

- **`expander.rkt`** — Hygienic macro expander using scope graphs and marks. This is the most complex module. Key ideas:
  - **Scope Graphs**: Each scope has bindings and a parent scope. Macro usages create a "disjoin" scope with two parents: one for use-site bindings and one for macro-introduced bindings. These are distinguished using marks on identifiers. Binding resolution involves traversing up the scope graph, popping marks on disjoin scopes, in search of a matching binding.
  - **Bindings**: `var-binding`, `keyword-binding`, `macro-binding`, `pattern-variable-binding` — each records its `site` identifier for LSP.
  - **`ExpanderState`** (a parameter): The expander accumulates static information in mutable tables. Each table is keyed by source span since LSP operations are in terms of source locations:
    - `resolutions` — maps each reference span to the binding(s) it resolved to, along with a snapshot of the scope at that point; used by goto-definition and autocomplete
    - `references` — maps each binding site span to all reference stx nodes that resolved to it; used by find-references
    - `bindings` — maps each binding site span to its binding; used to distinguish binding sites from reference sites and for semantic tokens
    - `stx-errors` — the set of all syntax errors encountered during expansion; published as diagnostics
  - **Two-pass definition expansion**: pass 1 discovers all bindings (enabling forward references), pass 2 expands expressions.
  - **`analyze!`** is the main entry point; returns an `ExpanderResult` containing information including tables found in `ExpanderState`.

- **`reader.rkt`** — Custom s-expression parser (`string->stx`, `string->stxs`) that produces `stx` trees with full source spans. Supports `()`, `[]`, dotted pairs, `#t`/`#f`, `'quote`, and `;` comments. Raises `exn:fail:parse` on errors.

- **`stx.rkt`** — Core data definitions. A `stx` wraps a `StxE` (symbol, number, bool, null, or cons pair) with a `span` (start/end `loc`, each a line, column, and character position) and hygiene `marks`. `span->srcloc` converts a span to a Racket srcloc.

- **`stx-quote.rkt`** — Quasiquote-style syntax construction helpers: `stx-quote` (pattern matching and quoting) and `stx-rebuild` (quasisyntax/loc for updating subexpressions while preserving source spans).

- **`lang/`** — `#lang treason`. `lang/reader.rkt` is the `#lang` entry point; `lang/read.rkt` reads the module body with treason's reader, padding the text with whitespace standing in for the `#lang` line so that spans locate the whole file; `lang/language.rkt` provides `#%module-begin`, which expands the file, reports its errors, and compiles it. The reader hands the body to `#%module-begin` as *text*, not as `stx`: a struct type is generated afresh for each module instantiation, so `stx` built by the reader is not the same type as the `stx` the expander uses one phase up.

- **`diagnostics.rkt`** — Collects every error from an expansion, in source order, and raises them as one exception, the way Typed Racket reports a module's type errors. The exception subtypes `exn:fail:syntax` and carries a srcloc per error, since a caller one phase away cannot recognise a treason-specific struct type.

- **`codegen.rkt`** — Compiles the expander's output (`XSExpr`) to Racket. The expander has already resolved hygiene and renamed every variable apart, so this is a structural translation. It only ever sees programs with no errors; a form it has no translation for is an internal error rather than code Racket would reject.

- **`constants.rkt`** — LSP protocol numeric constants (`SymbolKind/Variable`, `TextDocumentSyncKind/Full`, `DiagnosticSeverity/Error`, etc.)

- **`server-logging.rkt`** — File-based logging via `log-server-info` / `log-server-error` (writes to `logs.txt`).

### Test Files

- **`lsp-tests.rkt`** — Integration tests for LSP operations. Tests call `goto-definition`, `find-references`, and `autocomplete` directly on source strings.
- **`reader-tests.rkt`** — Unit tests for the reader.
- **`lang-tests.rkt`** — Integration tests for `#lang treason`: write a file, load it, and check what it printed or how it failed.

### Flow

Two things happen at different times:
1. **When the document changes** (didOpen/didChange): the server re-analyzes it (reader → expander → `analyze!`) and caches the `ExpanderResult`. Diagnostics are published back to the editor.
2. **When the editor requests goto-definition**: the server queries that cached result by looking up the node at the position, finding its resolutions, and returning the binding site spans.
