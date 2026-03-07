# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Is

**treason** is a tiny Lisp language with a full Language Server Protocol (LSP) implementation, written in Racket. The language supports `let`, `if`, `define`, `begin`, `block`, `let-syntax`, `define-syntax`, and `syntax-rules` macros. The LSP provides goto-definition, find-references, autocomplete, and document symbols.

## Commands

```bash
# Run all tests
raco test -p treason

# Run tests for a specific file
raco test lsp-tests.rkt
raco test reader-tests.rkt

# Run the language server (reads JSON-RPC from stdin)
racket server.rkt

# Install the package locally
raco pkg install --auto
```

## Architecture

The pipeline is: **source text → reader → stx → expander → ExpanderState → LSP operations**

### Modules

- **`stx.rkt`** — Core data definitions. A `stx` wraps a `StxE` (symbol, number, bool, null, or cons pair) with a `span` (start/end `loc`) and hygiene `marks`. All source locations live here.

- **`reader.rkt`** — Custom s-expression parser (`string->stx`) that produces `stx` trees with full source spans. Uses a mutable `pstate` parameter internally. Raises `exn:fail:parse` on errors.

- **`expander.rkt`** — Hygienic macro expander using scope graphs and marks. This is the most complex module. Key ideas:
  - **Scope graph**: `core-scope` → `scope` (linked-list parent chain) → `disjoin` (for macro hygiene, splits definition-site and use-site scopes via marks)
  - **Bindings**: `var-binding`, `keyword-binding`, `macro-binding`, `pattern-variable-binding` — each binding records its `site` identifier for LSP
  - **`ExpanderState`** (a parameter): accumulates three mutable tables during expansion — `span->stx` (surface span → stx), `resolutions` (ref span → `[Listof Resolution]`), `references` (def span → `[Listof ref-span]`). LSP queries read from this state after expansion.
  - `expand` is the main entry point; `expand-document` runs expansion and returns an `ExpanderResult`.

- **`server.rkt`** — JSON-RPC LSP server. `server%` is an object class; the main loop dispatches methods via `dynamic-send`. `proxy-client%` sends notifications (e.g. `textDocument/publishDiagnostics`) back to the editor. LSP helper functions (`goto-definition`, `find-references`, `autocomplete`) are plain functions that take an `ExpanderResult` and a cursor location.

- **`constants.rkt`** — LSP protocol numeric constants (`SymbolKind/Variable`, `TextDocumentSyncKind/Full`, etc.)

- **`server-logging.rkt`** — Logging via `log-server-info` / `log-server-error`.

- **`stx-quote.rkt`** — Quasiquote-style syntax construction helpers.

### Test Files

- **`lsp-tests.rkt`** — Integration tests for LSP operations. Tests call `goto-definition`, `find-references`, and `autocomplete` directly on source strings using helper functions `find-position` and `find-range` (which locate the Nth occurrence of a substring).
- **`reader-tests.rkt`** — Unit tests for the reader.

### Key Invariant

The expander is fault-tolerant: it continues expanding after errors (unbound variables, bad syntax) and emits `stx-error` nodes. This ensures LSP features work even in incomplete/broken programs.
