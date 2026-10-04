# RacketCon talk examples

The treason programs from the RacketCon talk (`racketcon/slides.rkt`), one file per example, numbered in the order they appear in the talk. Each file's header comment names its slide and says what to try in the editor.

Open them in VS Code with the [treason extension](https://github.com/quasarbright/treason-vscode) to try the IDE services. To see the errors from the command line, run a file with Racket:

```bash
racket racketcon-examples/03-multiple-errors.tsn
```

| File | Slide | `racket` reports |
| --- | --- | --- |
| `01-unfinished-body.tsn` | You Have Errors Most of the Time (unfinished body) | block must end in an expression |
| `02-top-down.tsn` | You Have Errors Most of the Time (top-down) | `average-grade` and `grade->letter` unbound |
| `03-multiple-errors.tsn` | Treason | both `unbound-ref`s unbound |
| `04-autocomplete-at-missing-expression.tsn` | Demo: Autocomplete at a Missing Expression | bad `my-let` use |
| `05-services-inside-bad-use.tsn` | Demo: Services Inside a Bad Macro Use | both `my-let` uses bad |
| `06-services-in-template.tsn` | Demo: Services in a Macro Template | bad `let` in the template |
| `07-where-services-come-from.tsn` | Where Do the Services Come From? | runs, prints `3` |
| `08-fault-tolerant-expansion.tsn` | Fault-Tolerant Expansion | bad `define1` use |
| `09-spec-driven-subexpression-expansion.tsn` | The Fix / The Hard Part / Spec-Driven Subexpression Expansion | bad `my-define` use, plus unbound names found by expanding its body |
| `10-cursor-driven-autocomplete.tsn` | Cursor-Driven Autocomplete | bad `let` |
| `11-sse-incomplete-context.tsn` | Limitations: SSE in Incomplete Context | bad `my-let` use, `x` unbound in its body |
| `12-sse-recursive-macro.tsn` | Limitations: SSE with Recursive Macros (before) | bad `my-cond` use |
| `13-sse-recursive-macro-annotated.tsn` | Limitations: SSE with Recursive Macros (after) | bad `my-cond` use |
| `14-sse-misalignment.tsn` | Limitations: SSE Can Misalign | bad `m` use |

## Differences from the slides

treason has no strings, structs, `void`, `sqr` or `sqrt`, and no function-definition shorthand, so some examples are adapted:

- **01 and 02:** functions are written `(define f (lambda (...) (block ...)))`. 01 defines its own `sqr`. 02 fakes the `student` and `report` structs with closures.
- **09:** `sqrt` and `sqr` stay as on the slide, so treason reports them unbound along with `x` and `y`.
- **11:** the slide reuses the `my-define` example. This file uses a `my-let` example instead, which shows a binding from inside the use going missing.
- **12 and 13:** the base case returns `#f` instead of `(void)`, and `x` is defined because the slide leaves it free.

The Rust and Racket screenshots and the `my-match` future-work example aren't included. The first two aren't treason, and treason can't express `my-match` yet.
