## error recovery

an expression that fails to expand will emit a stx-error object rather than a stx object, which will contain error and location information

the result of expansion will be stx that may have error children

## incremental reactive

right now, we have all information associated with stx as fields of a stx object. so far that's just stx-e and source span.

but in the future, we will need more information like scope, origin, parent, etc.

also, whenever anything changes, we re-process the entire program.

although this is a toy language, we want to prove that what we're doing can lend itself to efficient LSP support.

One mechanism for avoiding re-processing is incremental/reactive programming. We'd have passes to our compiler which look like this:

```racket
(define (analyze node-id db)
  (define children (get-children db node-id))
  ...)
```

where each pass takes in a node id and a db (which associates static information with node ids) and outputs some result and/or extends the db.

accessing other nodes like with get-children constructs a dependency graph. this informs the incremental framework what nodes need to be re-processed by each pass when the user edits that node in the program.

when a node needs to be re-processed, we should be able to just call `analyze` again with that node id and a possibly updated db, and that should be sufficient.

we can make a dsl around this to reduce how intrusive this framework is on our implementation

passes are recursive and often touch many nodes. not sure how this will interact with reactive re-evaluation.
parents depend on children, so obviously parents will need to be re-computed when children are updated. however, the parent might need to be re-processed before the child because the child processing may be under the context of the parent's (like expanding a let body with a new binding in store), assuming the pass is top-down. but sometimes this is not the case, like with the rhs of a let. you can re-process the rhs before re-processing the the let. I guess you can also re-process the body before re-processing the let, if you can re-access the context in which it was processed. so the let doesn't need to be updated? I'm confused.

let's think of an example.
```racket
(let ([x rhs]) body)
```
let's say we're doing an expander pass. If you edit
- x: body needs to be re-expanded, which means we need to re-expand the let. no need to re-expand rhs.
- rhs: rhs needs to be re-expanded, that's it
- body: rhs needs to be re-expanded, that's it

Another example
```racket
(let-syntax-rule ([pattern template]) body)
```
If you edit
- pattern/template (respectively): body and pattern/template needs to be re-expanded, but not template/pattern respectively. which means the whole let needs to be re-expanded, but not the template/pattern

```racket
(if cnd thn els)
```
- no child edits require re-expansion of others

In a dependency graph, how would we represent the fact that the parent needs to be re-expanded, but only some of the children? I guess we could use memoization and a more naive dependency graph, but I'd rather avoid that.

One edge case is that if you have a macro use with many deep sub-expressions and a deep child is edited, you can't just re-process the immediate parent. you need to go up to the use-site and re-expand it.

whatever you do, might want to start out with a simple non-macro prototype for this. maybe a type checker.

some languages use paths for node ids, like
```racket
(define_0_0 (f_0_1_0 x_0_1_1)_0_1 x_0_2)_0
```

but this might not play nice with expansion. maybe having an `expanded` segment in the path? need to think about this