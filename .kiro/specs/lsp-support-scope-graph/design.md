# Design Document: LSP Support for Scope Graph Expander

## Overview

This design adds LSP (Language Server Protocol) support to the hygienic macro expander in `private/scope-graph-prototype.rkt`. The expander uses scope graphs for hygiene, where scopes are vertices and parent relationships are edges with optional marks for macro expansion.

The key design principle is that only **surface syntax** (from the original source) needs LSP tracking. Macro-introduced syntax doesn't exist in the source file and shouldn't be navigable. We achieve this by:

1. Assigning unique integer IDs to surface syntax nodes during parsing
2. Assigning `#f` to macro-introduced nodes
3. Keying all LSP tables on these surface node IDs
4. Only recording entries when both reference and binding sites have IDs

## Architecture

All code lives in a single file: `private/scope-graph-prototype.rkt`. The LSP server (`server.rkt`) will call into this module.

```
┌─────────────────────────────────────────────────────────────────┐
│                        server.rkt                                │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────────────┐   │
│  │ goto-defn    │  │ find-refs    │  │ autocomplete         │   │
│  └──────┬───────┘  └──────┬───────┘  └──────────┬───────────┘   │
│         └─────────────────┼──────────────────────┘               │
└───────────────────────────┼──────────────────────────────────────┘
                            │
┌───────────────────────────┼──────────────────────────────────────┐
│                           │                                      │
│  private/scope-graph-prototype.rkt                               │
│                           │                                      │
│                    ┌──────▼──────┐                               │
│                    │ LSP Tables  │                               │
│                    │ references  │                               │
│                    │ resolutions │                               │
│                    │ id->stx     │                               │
│                    └──────▲──────┘                               │
│                           │                                      │
│                    ┌──────┴──────┐                               │
│                    │  Expander   │                               │
│                    │  (modified) │                               │
│                    └──────┬──────┘                               │
│                           │                                      │
│                    ┌──────▼──────┐                               │
│                    │ Scope Graph │                               │
│                    └─────────────┘                               │
│                                                                  │
└──────────────────────────────────────────────────────────────────┘
```

The architecture has two layers:
1. **LSP Server** (`server.rkt`): Handles protocol requests, calls expander module
2. **Expander + Tables** (`scope-graph-prototype.rkt`): Expander populates tables during expansion, provides query functions

## Components and Interfaces

### Data Definitions

```racket
;; ============================================================
;; Core Data Definitions
;; ============================================================

;; A Mark is a Symbol created by gensym.
;; Marks distinguish identifiers introduced at different macro expansion sites.

;; A Loc is a (loc Any Natural Natural)
(struct loc [source line column] #:transparent)
;; source : Any - identifies the source, often a file path
;; line : Natural - line number, zero-indexed
;; column : Natural - offset in that line, zero-indexed

;; A Span is a (span Loc Loc)
(struct span [start end] #:transparent)
;; start : Loc - start location
;; end : Loc - end location (exclusive)

;; A Surface_Node_ID is a Natural.
;; Unique identifier for surface syntax nodes, assigned during parsing.
;; Macro-introduced nodes have #f instead.

;; A Syntax is one of:
;; - (stx StxE Surface_Node_ID-or-#f Span-or-#f [Listof Mark])
;; - Number (unwrapped literals)
(struct stx [e id span marks] #:transparent)
;; e : StxE - the datum
;; id : (or/c Natural #f) - Surface_Node_ID, #f for macro-introduced
;; span : (or/c span #f) - source location
;;   - Surface syntax: span from the source
;;   - Macro-introduced (template): span from the template definition
;;   - Immediate macro result: span from the use site (for error reporting)
;; marks : [Listof Mark] - hygiene marks (most recent first)

;; A StxE is one of:
;; - Symbol (identifier)
;; - Number
;; - Boolean
;; - (cons Syntax Syntax)  ; improper list / pair
;; - '()                   ; empty list

;; An Identifier is a Syntax where (stx-e stx) is a Symbol.
;; Predicates and accessors:
(define (identifier? x) (and (stx? x) (symbol? (stx-e x))))
(define (identifier-symbol id) (stx-e id))
(define (identifier-marks id) (stx-marks id))
(define (identifier-id id) (stx-id id))
(define (identifier-span id) (stx-span id))

;; ============================================================
;; Scope Graph Data Definitions
;; ============================================================

;; An IdentifierKey is a (identifier-key Symbol [Listof Mark])
(struct identifier-key [symbol marks] #:transparent)
;; Used as keys in scope bindings. Only symbol and marks matter for
;; binding resolution - id and span are irrelevant.

;; identifier->key : Identifier -> IdentifierKey
(define (identifier->key id)
  (identifier-key (identifier-symbol id) (identifier-marks id)))

;; A Scope is one of:
;; - (core-scope [HashOf IdentifierKey Binding])
;; - (scope Scope [MutableHashOf IdentifierKey Binding])
;; - (disjoin Mark Scope Mark Scope)

(struct core-scope [bindings] #:transparent)
;; bindings : [HashOf IdentifierKey Binding]
;;   Immutable hash mapping identifier keys to bindings.
;;   The root of the scope graph containing core keywords.

(struct scope [parent bindings] #:transparent)
;; parent : Scope - edge to parent vertex
;; bindings : [MutableHashOf IdentifierKey Binding] - local bindings

(struct disjoin [mark1 scope1 mark2 scope2] #:transparent)
;; mark1 : Mark - definition-site mark
;; scope1 : Scope - definition-site scope
;; mark2 : Mark - use-site mark
;; scope2 : Scope - use-site scope
;; A vertex with two marked parent edges for macro expansion.

;; An unbound is a sentinel for failed resolution.
(struct unbound [] #:transparent)

;; A stx-error represents a syntax error during expansion.
(struct stx-error [who message stx sub-stx] #:transparent)
;; who : (or/c Symbol #f) - the form that detected the error (e.g., 'let)
;; message : String - error description
;; stx : Syntax - the syntax with the error
;; sub-stx : (or/c Syntax #f) - the specific sub-syntax causing the error, or #f

;; A Cursor is a surface Identifier used for autocomplete.
;; It has a gensym'd symbol name, a reserved ID (-1), a zero-width
;; span at the cursor position, and empty marks.
;; During autocomplete at a non-identifier position, we insert a cursor,
;; expand the program, and use get-names-in-scope on the cursor.

;; A Binding is one of:
;; - (var-binding Identifier)
;; - (keyword-binding Symbol)
;; - (macro-binding Identifier Syntax Scope)

(struct var-binding [site] #:transparent)
;; site : Identifier - the binding site identifier (has span for LSP)
;; Represents a variable binding (from let or define).

(struct keyword-binding [name] #:transparent)
;; name : Symbol - the keyword name ('let, 'define, etc.)
;; Represents a core keyword binding.

(struct macro-binding [site macrot scp] #:transparent)
;; site : Identifier - the binding site identifier (has span for LSP)
;; macrot : Syntax - the syntax-rules transformer
;; scp : Scope - definition-site scope
;; Represents a macro binding (from let-syntax or define-syntax).

;; ============================================================
;; LSP Table Data Definitions
;; ============================================================

;; A Resolution is a (resolution Binding-or-#f Identifier Scope)
(struct resolution [binding ref-id scp] #:transparent)
;; binding : (or/c Binding #f) - the resolved binding, or #f if unbound
;; ref-id : Identifier - the reference identifier (has marks for scope traversal)
;; scp : Scope - the scope vertex at the time of resolution

;; A Reference_Table is a (MutableHashEq Natural [Listof Natural])
;; Maps binding site ID to list of reference site IDs.
;; For efficient find-references.

;; A Resolutions_Table is a (MutableHashEq Natural [Listof Resolution])
;; Maps reference site ID to list of Resolutions.
;; Multi-valued because surface syntax can be duplicated by macros
;; and resolved multiple times (possibly to different bindings/scopes).
;; Used for:
;; - Goto-definition: extract span from the Binding's site field
;; - Autocomplete: use the ref-id's marks and scp to traverse scope graph

;; An ID_To_Stx_Table is a (MutableHashEq Natural Syntax)
;; Maps Surface_Node_ID to its stx object for span lookup.

;; ============================================================
;; Intermediate Representations
;; ============================================================

;; A with-disjoin is used between pass 1 and pass 2 of definition expansion.
(struct with-disjoin [stx scp] #:transparent)
;; stx : Syntax - syntax from pass 1
;; scp : Scope - disjoin scope to use in pass 2

;; A Projection is one of:
;; - '_ (hole)
;; - Number
;; - Syntax
;; - (cons Projection Projection)
;; - '()
;; Used to separate def-site and use-site portions of macro output.

;; A PatternEnv is a [HashOf IdentifierKey Syntax]
;; Maps pattern variables (by symbol+marks) to matched syntax during syntax-rules matching.
;; Uses IdentifierKey instead of Identifier to avoid false inequality from id/span fields.
```

### LSP Tables

The tracking tables are encapsulated in an `expander-state` struct, accessed via a parameter during expansion:

```racket
;; An expander-state contains all mutable state used during expansion.
(struct expander-state [references resolutions id->stx] #:transparent)
;; references : (MutableHashEq Natural (Listof Natural))
;;   Maps binding site ID to list of reference site IDs (multi-valued)
;; resolutions : (MutableHashEq Natural (Listof Resolution))
;;   Maps reference site ID to list of Resolutions
;; id->stx : (MutableHashEq Natural stx?)
;;   Maps surface node ID to its stx object (for span lookup)

;; current-expander-state : (Parameterof expander-state)
;; The current expander state, set by analyze!
(define current-expander-state (make-parameter #f))

;; make-expander-state : -> expander-state
;; Creates a fresh expander state with empty tables
(define (make-expander-state)
  (expander-state (make-hasheq) (make-hasheq) (make-hasheq)))

;; Recording functions (called by expander, use current-expander-state):
(define (record-resolution! ref-stx binding scp) ...)
(define (record-stx! stx) ...)
```

### Expander Result

The `analyze!` function returns an `expander-result` containing everything needed for LSP queries:

```racket
;; An expander-result is the output of analyze!
(struct expander-result [surface expanded errors state] #:transparent)
;; surface : stx? - the original surface syntax (with IDs assigned)
;; expanded : stx? - the fully expanded syntax
;; errors : (Listof stx-error?) - syntax errors found during expansion
;; state : expander-state - the final expander state with populated tables

;; analyze! : stx? -> expander-result
;; Main entry point. Initializes state, records surface syntax, expands, returns result.
(define (analyze! surface-stx)
  (define state (make-expander-state))
  (parameterize ([current-expander-state state])
    (record-all-stx! surface-stx)
    (define expanded (expand-expr surface-stx initial-scope))
    (define errors (find-stx-errors expanded))
    (expander-result surface-stx expanded errors state)))
```

### Exposed LSP Query Functions

These functions are exported from `scope-graph-prototype.rkt` for use by the LSP server. They take an `expander-result` and query its state.

```racket
;; expander-result? stx? -> (listof stx?)
;; Given a reference site, returns the binding sites it resolves to.
;; Filters out #f results (core keywords have no binding site).
;; Returns empty list if the node has no Surface_Node_ID or no recorded resolutions.
;; Multi-valued because surface syntax can be duplicated by macros.
(define (get-binding-sites-of result ref-stx) ...)

;; expander-result? stx? -> (listof stx?)
;; Given a binding site OR reference site, returns all reference sites.
;; If given a binding site, returns all references to that binding.
;; If given a reference site, first finds its binding sites, then finds
;; all references to those binding sites.
;; Includes the binding site itself (binding sites are self-references).
;; Returns empty list if the node has no Surface_Node_ID.
(define (get-reference-sites-of result stx) ...)

;; expander-result? stx? -> (setof symbol?)
;; Returns the names in scope at the given syntax node.
;; Uses the Resolution's ref-id marks and scope to traverse the graph.
;; If the node was resolved under multiple scopes (macro duplication),
;; returns the intersection of names from all scopes.
;; Returns empty set if no resolution information is recorded.
(define (get-names-in-scope result stx) ...)

;; expander-result? -> (listof stx?)
;; Returns all surface binding sites in the surface syntax tree.
;; Useful for document symbols / outline view.
(define (get-all-surface-binding-sites result) ...)
```

### Position-Based Query Functions

These higher-level functions take an `expander-result` and LSP positions (line+column), returning spans suitable for direct LSP protocol responses:

```racket
;; expander-result? Loc -> (or/c stx? #f)
;; Given an LSP position (line+column), find the innermost surface syntax node at that position.
(define (find-node-at-position result pos) ...)

;; expander-result? Loc -> (listof span?)
;; Goto definition: returns spans of binding sites for the identifier at position.
;; Filters out #f (core keywords have no definition to jump to).
(define (goto-definition result pos)
  (define node (find-node-at-position result pos))
  (if node
      (filter-map stx-span (get-binding-sites-of result node))
      '()))

;; expander-result? Loc -> (listof span?)
;; Find references: returns spans of all reference sites for the node at position.
;; Works for both binding sites and reference sites.
(define (find-references result pos)
  (define node (find-node-at-position result pos))
  (if node
      (filter-map stx-span (get-reference-sites-of result node))
      '()))

;; expander-result? Loc -> (setof symbol?)
;; Autocomplete: returns names in scope at position.
(define (autocomplete result pos)
  (define node (find-node-at-position result pos))
  (if node
      (get-names-in-scope result node)
      (seteq)))
```

## Data Models

### Surface Node ID Assignment

IDs are assigned during parsing in `reader.rkt`. The existing `stx.rkt` struct is augmented with `id` and `marks` fields. After parsing, `record-stx!` is called on all surface nodes (similar to how `record-parents!` works in `analyze!`).

```racket
(define current-id (make-parameter 0))

(define (next-id!)
  (begin0 (current-id)
    (current-id (add1 (current-id)))))

;; In reader.rkt, when creating stx nodes:
;; - Assign (next-id!) to each node's id field
;; - Initialize marks to '()

;; After parsing, before expansion:
(define (record-all-stx! root-stx)
  (let loop ([stx root-stx])
    (match stx
      [(stx e id span marks)
       (when id (record-stx! stx))
       (when (pair? e)
         (loop (car e))
         (loop (cdr e)))]
      [_ (void)])))
```

### Macro-Introduced Syntax

When a macro produces syntax from its template:
- Template literals get `id = #f` (macro-introduced, not navigable)
- Template literals keep their template span (for error messages pointing to macro definition)
- Pattern variable substitutions preserve their original IDs and spans (use-site syntax)
- The immediate/outermost result of macro expansion gets the use-site span **only if it has the def-site mark** (meaning it came from the template). If the result has the use-site mark (meaning it came from pattern variable substitution, even if it has `id = #f` from a nested macro), its span is left alone.

```racket
;; In project-def: template parts get #f for id, keep template span
(define (project-def tmpl penv)
  (match tmpl
    [(stx e id span marks)
     (cond
       [(and (identifier? tmpl) (hash-has-key? penv (identifier->key tmpl)))
        '_]  ; will be filled by use-site syntax
       [(pair? e)
        ;; Recurse into pair, but this node is template-originated
        (stx (cons (project-def (car e) penv)
                   (project-def (cdr e) penv))
             #f span marks)]  ; id=#f, keep template span
       [(identifier? tmpl)
        ;; Template identifier: macro-introduced
        (stx e #f span marks)]  ; id=#f, keep template span
       [else tmpl])]
    ...))

;; In combine-projections: use-site IDs/spans are preserved
(define (combine-projections proj1 proj2)
  (match* (proj1 proj2)
    [('_ proj2) proj2]  ; use-site syntax keeps its ID and span
    [(proj1 '_) proj1]  ; template syntax has id=#f, template span
    ...))

;; After macro expansion, if the outermost result has the def-site mark,
;; it came from the template and should get the use-site span for error reporting.
;; If it has the use-site mark, it came from pattern variable substitution
;; (possibly from a nested macro) and should keep its existing span.
(define (maybe-set-use-site-span result def-mark use-site-span)
  (match result
    [(stx e id span (cons (== def-mark) rest-marks))
     ;; Has def-site mark: came from template, use the use-site span
     (stx e id use-site-span (cons def-mark rest-marks))]
    [_ result]))  ; has use-site mark or other: keep existing span
```

### Marking Syntax

When marking syntax, we preserve IDs and spans but add marks:

```racket
;; mark-syntax : Syntax Mark -> Syntax
;; Adds a mark to all nodes, preserving IDs and spans
(define (mark-syntax stx mark)
  (match stx
    [(stx e id span marks)
     (stx (if (pair? e)
              (cons (mark-syntax (car e) mark)
                    (mark-syntax (cdr e) mark))
              e)
          id span (cons mark marks))]
    [_ stx]))  ; numbers pass through
```

### Table Data Structures

```racket
;; Helper to append to multi-valued hash
(define (hash-cons! ht key val)
  (hash-set! ht key (cons val (hash-ref ht key '()))))

;; Recording functions (use current-expander-state parameter)
(define (record-stx! stx)
  (define id (stx-id stx))
  (define state (current-expander-state))
  (when (and id state)
    (hash-set! (expander-state-id->stx state) id stx)))

(define (record-resolution! ref-stx binding scp)
  (define ref-id (stx-id ref-stx))
  (define state (current-expander-state))
  (when (and ref-id state)
    ;; Record the resolution for goto-definition and autocomplete
    (hash-cons! (expander-state-resolutions state) ref-id (resolution binding ref-stx scp))
    ;; If binding has a surface site, record in references table for find-references
    (define site (and binding (binding-site binding)))
    (define def-id (and site (stx-id site)))
    (when def-id
      (hash-cons! (expander-state-references state) def-id ref-id))))
```

### Query Function Implementations

```racket
;; expander-result? stx? -> (listof stx?)
;; Returns binding site stx for each resolution, filtering out #f (core keywords/unbound)
(define (get-binding-sites-of result ref-stx)
  (define ref-id (stx-id ref-stx))
  (define resolutions (expander-state-resolutions (expander-result-state result)))
  (if ref-id
      (filter-map
       (lambda (res)
         (define bnd (resolution-binding res))
         (and bnd (binding-site bnd)))
       (hash-ref resolutions ref-id '()))
      '()))

;; Binding -> (or/c Identifier #f)
;; Extract the binding site identifier from a binding
(define (binding-site bnd)
  (match bnd
    [(var-binding site) site]
    [(macro-binding site _ _) site]
    [(keyword-binding _) #f]))  ; keywords don't have surface binding sites

;; expander-result? stx? -> (listof stx?)
;; Given a binding site OR reference site, returns all reference sites.
(define (get-reference-sites-of result stx)
  (define id (stx-id stx))
  (define state (expander-result-state result))
  (define references (expander-state-references state))
  (define id->stx (expander-state-id->stx state))
  (if id
      ;; First check if this is a binding site (has entries in references table)
      (let ([direct-refs (hash-ref references id '())])
        (if (not (null? direct-refs))
            ;; It's a binding site - return all references to it
            (for/list ([ref-id direct-refs])
              (hash-ref id->stx ref-id))
            ;; It's a reference site - find binding sites, then their references
            (let* ([binding-sites (get-binding-sites-of result stx)]
                   [binding-ids (filter-map stx-id binding-sites)])
              (append-map
               (lambda (def-id)
                 (for/list ([ref-id (hash-ref references def-id '())])
                   (hash-ref id->stx ref-id)))
               binding-ids))))
      '()))

;; expander-result? stx? -> (setof symbol?)
;; Returns names in scope at the given syntax node.
;; Uses the resolution's scope and ref-id marks to traverse the graph.
(define (get-names-in-scope result stx)
  (define id (stx-id stx))
  (define resolutions (expander-state-resolutions (expander-result-state result)))
  (if id
      (let ([res-list (hash-ref resolutions id '())])
        (if (null? res-list)
            (seteq)
            (apply set-intersect
                   (for/list ([res res-list])
                     (scope->names (resolution-scp res)
                                   (identifier-marks (resolution-ref-id res)))))))
      (seteq)))

;; Scope [Listof Mark] -> (setof symbol?)
;; Traverse scope graph to collect names accessible with the given marks.
;; Only returns names whose binding keys have marks matching the current marks.
;; This mirrors scope-resolve: at each scope, we check bindings with matching marks,
;; and at disjoins, we drop the top mark when traversing the matching edge.
(define (scope->names scp marks)
  (match scp
    [(core-scope bindings)
     ;; Core scope: return symbols for bindings whose marks match current marks
     (for/seteq ([key (hash-keys bindings)]
                 #:when (equal? (identifier-key-marks key) marks))
       (identifier-key-symbol key))]
    [(scope parent bindings)
     ;; Regular scope: collect matching bindings, then recurse to parent
     (set-union
      (for/seteq ([key (hash-keys bindings)]
                  #:when (equal? (identifier-key-marks key) marks))
        (identifier-key-symbol key))
      (scope->names parent marks))]
    [(disjoin mark1 scope1 mark2 scope2)
     ;; Disjoin: only traverse the edge matching the top mark, dropping it
     (cond
       [(and (pair? marks) (eq? (car marks) mark1))
        (scope->names scope1 (cdr marks))]
       [(and (pair? marks) (eq? (car marks) mark2))
        (scope->names scope2 (cdr marks))]
       [else (seteq)])]))  ; no matching mark - can't traverse

;; expander-result? -> (listof stx?)
;; Returns all surface binding sites.
;; A binding site is a surface node that has entries in the references table.
(define (get-all-surface-binding-sites result)
  (define state (expander-result-state result))
  (define references (expander-state-references state))
  (define id->stx (expander-state-id->stx state))
  (for/list ([def-id (hash-keys references)])
    (hash-ref id->stx def-id)))
```

### ID-to-Stx Lookup

The `id->stx` table provides O(1) lookup from Surface_Node_ID to stx. This is populated by `record-stx!` before expansion.

For cases where we need to find a node by ID within a specific subtree (e.g., for debugging), we can traverse:

```racket
;; find-stx-by-id : Syntax Natural -> (or/c Syntax #f)
;; Find the syntax node with the given ID within a subtree
(define (find-stx-by-id root target-id)
  (let loop ([stx root])
    (match stx
      [(stx e id span marks)
       (cond
         [(equal? id target-id) stx]
         [(pair? e)
          (or (loop (car e))
              (loop (cdr e)))]
         [else #f])]
      [_ #f])))
```



## Correctness Properties

*A property is a characteristic or behavior that should hold true across all valid executions of a system—essentially, a formal statement about what the system should do. Properties serve as the bridge between human-readable specifications and machine-verifiable correctness guarantees.*

### Property 1: ID Uniqueness

*For any* s-expression converted to syntax, all assigned Surface_Node_IDs SHALL be unique (no two nodes share the same ID).

**Validates: Requirements 1.1**

### Property 2: ID Preservation Through Macro Expansion

*For any* macro expansion where use-site syntax is substituted for a pattern variable, the resulting syntax SHALL have the same Surface_Node_ID as the original use-site syntax.

**Validates: Requirements 1.3**

### Property 3: Macro-Introduced Nodes Are Not Tracked

*For any* macro expansion, syntax nodes originating from the macro template (not from pattern variable substitution) SHALL have `id = #f`, and SHALL NOT appear as keys in the Reference_Table or Resolutions_Table.

**Validates: Requirements 1.2, 2.3, 3.3, 3.4**

### Property 4: Surface Bindings Are Self-References

*For any* binding operation (`let`, `define`, `let-syntax`, `define-syntax`) where the binding site has a Surface_Node_ID, the Reference_Table SHALL contain an entry mapping that binding site's ID to itself (binding sites are self-references for LSP purposes).

**Validates: Requirements 2.1, 2.2, 2.5, 3.6**

### Property 5: Surface References Are Recorded

*For any* identifier resolution where the reference site has a Surface_Node_ID, the Resolutions_Table SHALL contain an entry mapping the reference site's ID to a Resolution containing the resolved Binding (or #f if unbound), the reference identifier, and the scope. If the binding site also has a Surface_Node_ID, the Reference_Table SHALL contain an entry mapping the binding site's ID to the reference site's ID.

**Validates: Requirements 3.1, 3.2**

### Property 6: Multi-Valued Tables Store All Occurrences

*For any* surface syntax node that is duplicated by a macro and processed multiple times (bound, resolved, or expanded), the corresponding table SHALL contain all occurrences for that Surface_Node_ID.

**Validates: Requirements 2.4, 3.5, 4.3**

### Property 7: Resolution Recording for Identifiers

*For any* surface identifier that is resolved, the Resolutions_Table SHALL contain an entry with the scope vertex active during resolution.

**Validates: Requirements 4.1, 4.2**

### Property 8: Goto-Definition Round Trip

*For any* surface reference site with a Surface_Node_ID that resolves to a surface binding site, calling goto-definition on the reference's position SHALL return the span of the binding site.

**Validates: Requirements 5.1, 5.2, 5.3**

### Property 9: Find-References Completeness

*For any* surface binding site, calling find-references SHALL return the spans of all surface reference sites that resolve to that binding, including the binding site itself.

**Validates: Requirements 6.2, 6.3, 6.4**

### Property 10: Autocomplete Scope Traversal

*For any* surface syntax node with a recorded Resolution, the autocomplete result SHALL include exactly the names reachable by traversing parent edges in the scope graph from the Resolution's scope, respecting mark constraints for disjoin vertices based on the Resolution's ref-id marks.

**Validates: Requirements 4.4, 7.3**

### Property 11: Autocomplete Intersection for Duplicated Syntax

*For any* surface syntax node that was resolved multiple times (due to macro duplication), the autocomplete result SHALL be the intersection of names from all recorded Resolutions.

**Validates: Requirements 7.4**

### Property 12: Hygiene Preservation in LSP

*For any* program where a macro introduces a binding with the same symbol as a surface binding, surface references to that symbol SHALL resolve to the surface binding (not the macro-introduced one), and this SHALL be reflected in the Reference_Table.

**Validates: Requirements 8.1, 8.3**

### Property 13: Autocomplete Soundness

*For any* name returned by autocomplete at a given position, inserting that name as an identifier at that position SHOULD NOT result in an unbound variable error during expansion.

Note: This property may not hold in the presence of certain macros that inspect or manipulate their arguments in ways that affect binding (e.g., macros that conditionally bind names based on runtime values or that use unhygienic capture).

**Validates: Requirements 7.1, 7.3**

## Error Handling

### Fault-Tolerant Expansion

Like `expander.rkt`, the expander should be fault-tolerant: syntax errors produce `stx-error` sentinel values in the AST rather than halting expansion. This allows:
- Partial expansion results for LSP features
- Multiple errors to be reported at once
- LSP features to work on code with errors

```racket
;; Errors are caught and returned as values
(define-syntax-rule (with-stx-error-handling body ...)
  (with-handlers ([stx-error? (lambda (err) err)])
    body ...))

;; Example: unbound variable becomes stx-error in the AST
;; (let ([x 1]) y) expands to (let ([x 1]) <stx-error: unbound y>)
```

### Cursor for Autocomplete

When the cursor position is inside or at the end of an existing identifier, we use that identifier directly for autocomplete (no cursor insertion needed). The identifier already has resolution information recorded.

When the cursor position is not at an identifier (e.g., in whitespace or after an open paren), we insert a cursor at the user's cursor position. The cursor is a normal surface identifier - it has a reserved ID (-1), a zero-width span at the cursor position, and empty marks. Its name is gensym'd to avoid collisions:

```racket
;; Reserved ID for cursor
(define cursor-id -1)

;; Create a cursor identifier at the given position
;; It's a normal surface identifier, just with a gensym'd name and reserved ID
(define (make-cursor pos)
  (define zero-span (span pos pos))  ; zero-width span at cursor
  (stx (gensym 'cursor) cursor-id zero-span '()))

;; Autocomplete at position
(define (autocomplete-at-position result pos)
  (define node (find-node-at-position result pos))
  (cond
    ;; If position is at an identifier, use its recorded resolution
    [(and node (identifier? node))
     (get-names-in-scope result node)]
    ;; Otherwise, insert cursor and re-expand
    [else
     (define cursor (make-cursor pos))
     (define with-cursor (insert-cursor-at (expander-result-surface result) pos cursor))
     (define cursor-result (analyze! with-cursor))
     ;; Cursor is a surface identifier with id=-1, so get-names-in-scope works
     (get-names-in-scope cursor-result cursor)]))
```

The cursor approach is needed for positions that don't correspond to existing identifiers because:
- It handles positions inside macro-generated code correctly
- It respects hygiene (the cursor gets marks like any other identifier)
- It works even for positions that don't correspond to existing syntax

### Invalid Positions

When an LSP query specifies a position that doesn't correspond to any syntax node:
- Return an empty result (not an error)
- This handles whitespace, comments, and out-of-bounds positions

### Unbound Identifiers

When a reference doesn't resolve to any binding:
- The Resolutions_Table still contains an entry with `binding = #f` and the scope (for autocomplete)
- The Reference_Table will have no entry for that ID (no binding site to point to)
- Goto-definition returns empty result
- Find-references returns empty result
- Autocomplete still works (uses the scope from the Resolution)

### Malformed Programs

When expansion fails due to syntax errors:
- Tables may be partially populated up to the point of failure
- LSP queries should still work for the successfully expanded portions
- `stx-error` values in the AST indicate where errors occurred

### Macro Expansion Errors

When a macro application fails:
- The use-site syntax should still have scope information recorded
- References within the macro arguments may be partially tracked
- The error is represented as `stx-error` in the expanded output

## Testing Strategy

### Unit Tests (in scope-graph-prototype.rkt)

Light unit tests in the expander module to verify basic functionality:

1. **Basic expansion**: Verify the expander still works correctly with the new stx struct
2. **Table population**: Verify tables are populated for simple cases like `(let ([x 1]) x)`
3. **Hygiene test case**: The Fig. 17 example from the prototype

### LSP Tests (in lsp-tests.rkt)

Most tests should be in `lsp-tests.rkt` since they're implementation-agnostic and test the LSP-level behavior.

#### Targeted Tests

Specific test cases for each LSP feature:

1. **Goto-definition**: Various binding forms (let, define, let-syntax, define-syntax)
2. **Find-references**: Forward references, multiple references, self-references
3. **Autocomplete**: Names in scope at various positions
4. **Nested scopes**: Shadowing behavior
5. **Macro hygiene**: Surface references resolve to surface bindings
6. **Macro-introduced bindings**: Don't appear in LSP results
7. **Duplicated syntax**: Multi-valued table behavior
8. **Error cases**: Unbound identifiers, malformed programs

#### Sample Program Pool Tests

A curated pool of sample programs covering interesting cases, with consistency checks run over all of them:

Sample programs include:
- Simple bindings (let, define)
- Nested scopes with shadowing
- Forward references in blocks
- Various macro patterns (identity, or-macro, nested macros)
- The Fig. 17 hygiene example
- Macros that introduce bindings
- Macros that duplicate use-site syntax
- Programs with errors (unbound vars, bad syntax)
- Edge cases from `interesting-cases.md`

Consistency checks run on each program:
- **Round-trip goto-definition**: For each reference, goto-definition returns a span containing a binding for that name
- **Find-references completeness**: For each binding, find-references returns all references that resolve to it
- **Autocomplete soundness**: For each position, names returned by autocomplete don't cause unbound errors when inserted
- **Table consistency**: All resolutions have valid scopes, all reference entries point to valid IDs
