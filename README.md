tiny-lispy-lsp
==============
tiny lispy language with a language server

## Grammar

```
expr := atom
      | (let ([var expr]) expr)
```