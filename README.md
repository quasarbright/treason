tiny-lispy-lsp
==============
tiny lispy language with a language server

## Grammar

```
expr := atom
      | var
      | (let ([var expr]) expr)
      | (if expr expr expr)
      | (let-syntax-rule ([(var expr ...) expr]) expr)
      | (var expr ...)
```

only macro applications for now