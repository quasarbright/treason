## error recovery

an expression that fails to expand will emit a stx-error object rather than a stx object, which will contain error and location information

the result of expansion will be stx that may have error children

## incremental reactive

Obsolete. See [query based](#query-based).

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


## goto definition and references with macros

as of writing this, we run goto definition and references on expanded code, which means we can't use them on macro names since their definition and usage disappears during expansion.

here are examples of desired behavior that is currently not supported

```racket
(define-syntax-rule (m x) x)
(m)
(m)
```
goto definition on either m should take you to its position in line 1

find references on line 1 should take you to its position on lines 2 and 3

in order to accomplish this, we must record information about macro definition and use sites during expansion and make this information available to the language server. we also don't want macro-introduced syntax to be falsely captured by these operations

```racket
(let-syntax-rule ([(bind-x body) (let ([x 1]) body)])
  (let-syntax-rule ([(ref-x) x])
    (bind-x (ref-x))))
```

we don't want goto definition on `(ref-x)` to bring us to the template of `bind-x` since that isn't on the surface. This might be the current behavior depending on how macro-introduced spans work.

In general, we want to distinguish between surface and macro-introduced syntax, and have knowledge of the surface syntax before and after expansion. before for macros and after for variables that may be bound only in expanded code. I don't want to keep adding fields to `stx` so maybe we should do node ids and mappings from node ids to info, moving towards something like [incremental reactive](#incremental-reactive)

## query based

parser is a `string -> (values id (hash id ast) (hash id span)`. returns root node, red tree, and mapping from node to source span.
queries are functions, usually keyed by node id
query system tracks a dependency graph of queries and caches results. queries are invalidated when relevant parts of the source changes. there should also be garbage collection to prevent space leaks.

### parser
```
ast := var
     | lit
     | lambda([ast-id], ast-id)
	 | fun-app(ast-id, [ast-id])
     | let(ast-id, ast-id, ast-id)
     | let-stx(sexpr-id, sexpr-id, ast-id)
     | macro-app(ast-id, [sexpr-id])

sexpr := var
       | lit
       | [sexpr-id]

id := path
path := [segment]
segment := top-children
         | index(nat)
		 | lambda-args | lambda-body
		 | fun-app-head | fun-app-args
         | let-rhs | let-lhs | let-body
		 | let-stx-pattern | let-stx-template | let-stx-body
         | macro-app-head | macro-app-args
         | expanded
```
How we assign IDs to nodes is important, since when IDs change, queries involving those nodes are invalidated. We want stable ids that change very little as the program is edited.
Path-based identification doesn't work well on pure s-expressions, where inserts, transposes, etc. are common like in an expression body, since they affect the ids of neighbors. But on an AST, the path segments are "more semantic", and thus are more stable.
In this design, editing nodes doesn't affect their ascendants or siblings, but editing/moving nodes affects their descendants and, in some cases, siblings.
Macros introduce challenges:
- Without a grammar, we have to use index-based ids for macro applications (most of surface syntax), which are unstable. This will lead to lots of unnecessary cache invalidations on edits
- We need to map macro use syntax back to its origin after expanding the macro to support operations like "find references"
- Since expansion may have side effects, we cannot lazily/incrementally expand. Thus, like parsing, it will happen before queries and potentially invalidate them.
However, grammars help:
- We can dynamically generate new types of AST nodes, constructors for them, and path segments, having a more semantics-stable node identification than raw sexpr indices
- Assuming hygiene, we only need to expand to detect syntax errors on procedurally macro-generated syntax. Binding resolution can happen on macro uses for macros with grammar + binding.
- Without procedural macros that can construct arbitrary syntax, we don't even need to expand as long as we check every template of every macro. Kind of like type checking but grammar. We can even have procedural macros as long as their output is statically checkable without expanding. But if we add procedural IDE hooks like what racket does for hover info, we'll need to expand.
### Queries
example: go to definition
```
;; node-id -> (or #f node-id)
goto-definition(node-id):
  match node-e(node-id)
  | x ->
    parent-id = get-parent(node-id)
    parent-id and goto-definition/help(node-id, x)

;; node-id symbol -> (or #f node-id)
goto-definition/help(node-id, name):
  match node-e(node-id)
  | let x = _ in _ ->
    if name == node-e(x)
    then x
    else get-parent(node-id) and goto-definition/help(get-parent(node-id), name)
```
Instead of a top-down, environment-accumulating traversal, we start at the node in question and traverse _up_ the tree until we hit its binding site.
Some queries are updated on each change, like those for diagnostics.
### Query System
When the program is edited, some queries need to be invalidated.