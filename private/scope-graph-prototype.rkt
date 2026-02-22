#lang racket

(provide expand)

(require racket/hash)

;; Grammar of the language accepted by this expander:
;;
;; var, mname, pvar are ids
;;
;; expr := number
;;       | var
;;       | (block def ...)
;;       | (let ([var expr]) expr)
;;       | (let-syntax ([mname macrot]) expr)
;;       | (mname ustx ...)
;; def := (define-syntax mname macrot)
;;      | (define var expr)
;;      | (begin def ...)
;;      | (#%expression expr)
;;      | (mname ustx ...)
;;
;; macrot := (syntax-rules (id ...) [(_ pvar ...) tmpl] ...)
;;
;; ustx := var | expr
;;
;; tmpl := (tmpl . tmpl)
;;       | number
;;       | id
;;       | id that is pvar

;; Projections of syntax:
;;
;; proj := (proj . proj)
;;       | number
;;       | id
;;       | _

;; ============================================================
;; Data Definitions
;; ============================================================

;; A Mark is a Symbol created by gensym.
;; Marks are used to distinguish identifiers introduced at different
;; macro expansion sites. Each macro application generates fresh
;; definition-site and use-site marks.

;; A Syntax is one of:
;; - Number
;; - Identifier
;; - (cons Syntax Syntax)
;; - '()
;; Represents syntax objects in the language being expanded.

;; An Identifier represents a name with its expansion history.
(struct identifier [symbol marks]
  ;; symbol : Symbol
  ;;   The underlying symbol name.
  ;; marks : [Listof Mark]
  ;;   Stack of marks accumulated during macro expansion.
  ;;   The first element is the most recent (top) mark.
  #:transparent)

;; A Binding is one of:
;; - Symbol           ; a core keyword like 'let, 'define, 'block, etc.
;; - Symbol           ; a gensym'd variable name (the "renamed" variable)
;; - macro-closure    ; a user-defined macro
;; Represents what an identifier resolves to in a scope.
;; Note: Core keywords and renamed variables are both symbols;
;; they are distinguished by context (core keywords are from initial-scope,
;; renamed variables are gensym'd during expansion).

;; A macro-closure represents a macro transformer along with
;; the scope in which it was defined.
(struct macro-closure [macrot scp]
  ;; macrot : Syntax
  ;;   The syntax-rules transformer expression.
  ;; scp : Scope
  ;;   The scope where the macro was defined, used as the
  ;;   definition-site parent when the macro is applied.
  #:transparent)

;; A PatternEnv is a [HashOf Identifier Syntax]
;; Maps pattern variables to the syntax they matched during
;; syntax-rules pattern matching.

;; A Projection is one of:
;; - '_ (hole)
;; - Number
;; - Identifier
;; - (cons Projection Projection)
;; - '()
;; Represents a "view" of a template with holes where certain
;; parts have been masked out. Used to separate definition-site
;; and use-site portions of macro output for differential marking.

;; ------------------------------------------------------------
;; Scopes (Scope Graph Vertices)
;; ------------------------------------------------------------

;; This expander uses a scope graph instead of scope sets.
;; Scopes are vertices in the graph. Each scope has bindings and
;; edges to parent scopes. Resolution traverses parent edges upward.
;;
;; For macro expansion, "disjoin" vertices have two parents with
;; marked edges: one for definition-site and one for use-site.
;; An identifier can only traverse a marked edge if it has the
;; corresponding mark on top of its mark stack.

;; A Scope is one of:
;; - core-scope
;; - scope
;; - disjoin

;; A core-scope is the root of the scope graph containing
;; bindings for core keywords.
(struct core-scope [bindings]
  ;; bindings : [HashOf Identifier Binding]
  ;;   Immutable hash mapping identifiers to their bindings.
  #:transparent)

;; A scope is a regular scope vertex with a parent edge and
;; local bindings. Created when entering scoping forms like let or block.
(struct scope [parent bindings]
  ;; parent : Scope
  ;;   The enclosing scope (edge to parent vertex).
  ;; bindings : [MutableHashOf Identifier Binding]
  ;;   Local bindings in this scope.
  #:transparent)

;; A disjoin is a scope vertex with two marked parent edges,
;; created during macro expansion. It allows identifiers from
;; the macro's definition site and use site to resolve in their
;; respective scopes.
(struct disjoin [mark1 scope1 mark2 scope2]
  ;; mark1 : Mark
  ;;   The definition-site mark.
  ;; scope1 : Scope
  ;;   The definition-site scope (where the macro was defined,
  ;;   extended with a fresh scope for introduced definitions).
  ;; mark2 : Mark
  ;;   The use-site mark.
  ;; scope2 : Scope
  ;;   The use-site scope (where the macro was applied).
  #:transparent)

;; An unbound is a sentinel value indicating that resolution failed.
(struct unbound [] #:transparent)

;; ------------------------------------------------------------
;; Pass 1/2 Intermediate Representation
;; ------------------------------------------------------------

;; A with-disjoin is an intermediate representation used between
;; pass 1 and pass 2 of definition expansion.
;; When a macro expands in pass 1, the result continues through pass 1
;; to discover any definitions it introduced. In pass 2, we need to
;; remember to use the disjoin scope (not the original/block scope) when
;; expanding expressions in that macro's output.
(struct with-disjoin [stx scp]
  ;; stx : Syntax
  ;;   The syntax from pass 1 expansion.
  ;; scp : Scope
  ;;   The disjoin scope to use in pass 2.
  #:transparent)

;; ============================================================
;; Top-Level Entry Point
;; ============================================================

;; expand : SExpression -> SExpression
;; Expands an s-expression program, returning fully-expanded output.
(define (expand e)
  (parameterize ([gensym-ctr 0])
    (expand-expr (sexpr->syntax e) initial-scope)))

;; ============================================================
;; Expression Expansion
;; ============================================================

;; expand-expr : Syntax Scope -> Syntax
;; Expands an expression in the given scope, producing fully-expanded syntax.
(define (expand-expr expr scp)
  (match expr
    [(? number? n) n]
    [(? identifier? id)
     #:when (symbol? (scope-resolve scp id))
     (scope-resolve scp id)]
    [`(,block-id ,def* ...)
     #:when (eq? 'block (scope-resolve scp block-id))
     (define scp^ (new-scope scp))
     (define def*^ (expand-def*-pass1 def* scp^))
     (define def*^^ (expand-def*-pass2 def*^ scp^))
     `(block . ,def*^^)]
    [`(,let-id ([,x ,e]) ,b)
     #:when (eq? 'let (scope-resolve scp let-id))
     (define e^ (expand-expr e scp))
     (define x^ (gensym (identifier-symbol x)))
     (define scp^ (new-scope scp))
     (scope-bind! scp^ x x^)
     (define b^ (expand-expr b scp^))
     `(let ([,x^ ,e^]) ,b^)]
    [`(,let-syntax-id ([,mname ,macrot])
                      ,body)
     #:when (eq? 'let-syntax (scope-resolve scp let-syntax-id))
     (expand-expr body (scope scp (hash mname (macro-closure macrot scp))))]
    [`(,mname ,ustx* ...)
     (define-values (marked-stx disjoined-scp) (expand-macro mname expr scp))
     (expand-expr marked-stx disjoined-scp)]))

;; ============================================================
;; Definition Expansion (Two-Pass)
;; ============================================================

;; Definition contexts (like block) use two-pass expansion:
;; Pass 1: Discover all bindings (variables and macros) and expand macros.
;; Pass 2: Expand all expressions now that all bindings are known.
;; This allows forward references within a definition context.

;; expand-def*-pass1 : [Listof Syntax] Scope -> [Listof (or Syntax with-disjoin)]
;; First pass over a list of definitions.
(define (expand-def*-pass1 def* scp)
  (for/list ([def def*])
    (expand-def-pass1 def scp)))

;; expand-def*-pass2 : [Listof (or Syntax with-disjoin)] Scope -> [Listof Syntax]
;; Second pass over a list of definitions.
(define (expand-def*-pass2 def* scp)
  (for/list ([def def*])
    (expand-def-pass2 def scp)))

;; expand-def-pass1 : Syntax Scope -> (or Syntax with-disjoin)
;; First pass of definition expansion: discovers bindings and expands macros.
;; Returns either expanded syntax or a with-disjoin wrapper.
(define (expand-def-pass1 def scp)
  (match def
    [`(,define-id ,var ,expr)
     #:when (eq? 'define (scope-resolve scp define-id))
     (define var^ (gensym (identifier-symbol var)))
     (scope-bind! scp var var^)
     `(define ,var^ ,expr)]
    [`(,define-syntax-id ,var ,macrot)
     #:when (eq? 'define-syntax (scope-resolve scp define-syntax-id))
     (scope-bind! scp var (macro-closure macrot scp))
     `(begin)]
    [`(,begin-id ,def* ...)
     #:when (eq? 'begin (scope-resolve scp begin-id))
     (define def*^ (expand-def*-pass1 def* scp))
     `(begin . ,def*^)]
    [`(,#%expression-id ,expr)
     #:when (eq? '#%expression (scope-resolve scp #%expression-id))
     `(#%expression ,expr)]
    [`(,mname ,ustx* ...)
     (define-values (marked-stx disjoined-scp) (expand-macro mname def scp))
     ;; the reason for with-disjoin is because without it, we'd expand the
     ;; rhs of the macro-generated definition in the block scope instead of the
     ;; macro-use's disjoin scope. And since it came from the macro,
     ;; we need to expand it under the disjoin scope.
     (with-disjoin (expand-def-pass1 marked-stx disjoined-scp) disjoined-scp)]))

;; expand-def-pass2 : (or Syntax with-disjoin) Scope -> Syntax
;; Second pass of definition expansion: expands all expressions.
;; The scp argument is the default scope; with-disjoin wrappers
;; override it with their stored disjoin scope.
(define (expand-def-pass2 def scp)
  (match def
    [`(define ,var ,expr)
     `(define ,var ,(expand-expr expr scp))]
    [`(begin ,def* ...)
     (define def*^ (expand-def*-pass2 def* scp))
     `(begin . ,def*^)]
    [`(#%expression ,expr)
     `(#%expression ,(expand-expr expr scp))]
    [(with-disjoin stx scp)
     (expand-def-pass2 stx scp)]))

;; ============================================================
;; Macro Expansion
;; ============================================================

;; expand-macro : Identifier Syntax Scope -> (values Syntax Scope)
;; Expands a macro application.
;; Returns the expanded syntax (with marks applied) and a disjoin scope
;; for continuing expansion.
;;
;; The expansion process:
;; 1. Look up the macro-closure for the macro name.
;; 2. Match the input against syntax-rules patterns to get a pattern env and template.
;; 3. Create fresh definition-site and use-site marks.
;; 4. Project the template into def-site and use-site portions.
;; 5. Mark each projection with its corresponding mark.
;; 6. Combine the projections back together.
;; 7. Create a disjoin scope with marked edges to def-site and use-site scopes.
(define (expand-macro mname expr use-scp)
  (match-define
    (macro-closure macrot def-scp)
    (scope-resolve use-scp mname))
  (define-values (penv tmpl)
    (select-syntax-rule macrot expr def-scp use-scp))
  (define def-mark (fresh-def-mark))
  (define use-mark (fresh-use-mark))
  (define marked-stx (combine-projections
                      (mark-syntax (project-def tmpl penv) def-mark)
                      (mark-syntax (project-use tmpl penv) use-mark)))
  ;; we need a new def scope here so we don't pollute the
  ;; macro definition site with definitions generated by the macro.
  ;; like if the macro defines a temporary variable.
  ;; it needs a "sandbox" scope just for this expansion.
  ;; but we just use the use-scp directly because
  ;; we DO want use-site-defined identifiers bound directly
  ;; in the use-site scope so it can be referenced in sibling expressions.
  (define introduced-defn-scp (new-scope def-scp))
  (define disjoined-scp (disjoin def-mark introduced-defn-scp
                                 use-mark use-scp))
  (values marked-stx disjoined-scp))

;; ============================================================
;; Template Projection and Combination
;; ============================================================

;; The projection mechanism separates a template into two "views":
;; - project-def: keeps template structure, replaces pattern variable positions with '_
;; - project-use: keeps pattern variable substitutions, replaces template literals with '_
;;
;; Each projection is then marked with its corresponding mark (def or use).
;; Finally, combine-projections merges them back together.
;;
;; This ensures that identifiers from the macro definition (template literals)
;; get the definition-site mark, while identifiers from the macro use
;; (substituted from pattern variables) get the use-site mark.

;; project-def : Syntax PatternEnv -> Projection
;; Creates the definition-site projection of a template.
;; Pattern variable positions become '_, everything else is kept.
(define (project-def tmpl penv)
  (match tmpl
    [(cons a d)
     (cons (project-def a penv) (project-def d penv))]
    [(? identifier? pvar)
     #:when (hash-has-key? penv pvar)
     '_]
    [_ tmpl]))

;; project-use : Syntax PatternEnv -> Projection
;; Creates the use-site projection of a template.
;; Pattern variables are replaced with their matched syntax,
;; everything else becomes '_.
(define (project-use tmpl penv)
  (match tmpl
    [(? identifier? pvar)
     #:when (hash-has-key? penv pvar)
     (hash-ref penv pvar)]
    [(cons a d)
     (cons (project-use a penv) (project-use d penv))]
    ['() '()]
    [_ '_]))

;; combine-projections : Projection Projection -> Syntax
;; Merges two projections back into a single syntax object.
;; '_ in one projection is filled by the corresponding part of the other.
(define (combine-projections proj1 proj2)
  (match* (proj1 proj2)
    [('_ proj2) proj2]
    [(proj1 '_) proj1]
    [(same same) same]
    [((cons proj1a proj1d) (cons proj2a proj2d))
     (cons (combine-projections proj1a proj2a)
           (combine-projections proj1d proj2d))]))

;; ============================================================
;; Syntax-Rules Matching
;; ============================================================

;; select-syntax-rule : Syntax Syntax Scope Scope -> (values PatternEnv Syntax)
;; Selects the first matching clause from a syntax-rules transformer.
;; Returns the pattern environment (bindings of pattern variables) and
;; the template to instantiate.
(define (select-syntax-rule macrot expr def-scp use-scp)
  (match macrot
    [`(,syntax-rules-id (,literal-id* ...)
                        ,clause* ...)
     (define is-literal? (make-is-literal? literal-id*))
     (define literal-match? (make-literal-match? def-scp use-scp))
     (try-clauses clause* expr is-literal? literal-match?)]))

;; try-clauses : [Listof Clause] Syntax (Id -> Bool) (Id Id -> Bool) -> (values PatternEnv Syntax)
;; Tries each clause in order until one matches.
;; A Clause is (list Pattern Template).
(define (try-clauses clauses expr is-literal? literal-match?)
  (match clauses
    [(cons `[,pat ,tmpl] rest)
     (define maybe-penv (match-top-pattern pat expr is-literal? literal-match?))
     (if maybe-penv
         (values maybe-penv tmpl)
         (try-clauses rest expr is-literal? literal-match?))]
    ['() (error 'syntax-rules "no pattern matched")]))

;; match-top-pattern : Pattern Syntax (Id -> Bool) (Id Id -> Bool) -> (or PatternEnv #f)
;; Matches a top-level pattern against syntax.
;; The car of both pattern and syntax is the macro name (ignored per syntax-rules semantics).
;; Returns a PatternEnv on success, #f on failure.
;;
;; Note: This implementation omits ellipses and other datatypes
;; not supported in the input language (vectors, booleans, etc.).
(define (match-top-pattern pat expr is-literal? literal-match?)
  (match-pattern (cdr pat) (cdr expr) is-literal? literal-match?))

;; match-pattern : Pattern Syntax (Id -> Bool) (Id Id -> Bool) -> (or PatternEnv #f)
;; Matches a pattern against syntax.
;; Returns a PatternEnv mapping pattern variables to matched syntax on success,
;; or #f if the pattern doesn't match.
(define (match-pattern pat expr is-literal? literal-match?)
  (match* (pat expr)
    ;; literal-id: must match via literal-match?
    [((? identifier? lit) (? identifier? target-id))
     #:when (is-literal? lit)
     (and (literal-match? lit target-id)
          (hash))]
    ;; pvar: matches any syntax, binds it
    [((? identifier? pvar) stx)
     #:when (not (is-literal? pvar))
     (hash pvar stx)]
    ;; datum literal: must be equal
    [((? number? v) v)
     (hash)]
    ;; cons: match both parts and combine environments
    [(`(,pa . ,pd) `(,ea . ,ed))
     (let ([resa (match-pattern pa ea is-literal? literal-match?)])
       (and resa
            (let ([resd (match-pattern pd ed is-literal? literal-match?)])
              (and resd
                   (hash-union resa resd)))))]
    [('() '()) (hash)]
    [(_ _) #f]))

;; make-is-literal? : [Listof Identifier] -> (Identifier -> Boolean)
;; Creates a predicate that checks if an identifier is a literal
;; (using bound-identifier=? comparison).
(define (make-is-literal? literal-id*)
  (lambda (id)
    (memf (lambda (x) (bound-identifier=? id x)) literal-id*)))

;; make-literal-match? : Scope Scope -> (Identifier Identifier -> Boolean)
;; Creates a predicate for matching literals in syntax-rules.
;; A literal in the pattern matches an identifier in the input if:
;; - Both are bound and resolve to the same binding, OR
;; - Both are unbound and have the same symbol name.
;; This implements free-identifier=? semantics for syntax-rules literals.
(define (make-literal-match? def-scp use-scp)
  (lambda (literal-id target-id)
    (define literal-binding (scope-resolve def-scp literal-id))
    (define target-binding (scope-resolve use-scp target-id))
    (or (and (not (unbound? literal-binding)) (not (unbound? target-binding))
             (eq? literal-binding target-binding))
        (and (unbound? literal-binding) (unbound? target-binding)
             (eq? (identifier-symbol literal-id) (identifier-symbol target-id))))))

;; ============================================================
;; Keywords
;; ============================================================

;; keywords : [Listof Symbol]
;; The core keywords of the language that have special meaning
;; and are bound in the initial scope.
(define keywords '(let let-syntax syntax-rules define
                    define-syntax block begin #%expression))

;; ============================================================
;; Scope Operations
;; ============================================================

;; new-scope : Scope -> Scope
;; Creates a new scope vertex with the given parent and empty bindings.
(define (new-scope parent)
  (scope parent (make-hash)))

;; scope-resolve : Scope Identifier -> (or Binding unbound)
;; Resolves an identifier by traversing parent edges in the scope graph.
;; For disjoin vertices, follows the edge matching the identifier's top mark.
(define (scope-resolve scp id)
  (match scp
    [(core-scope core-bindings)
     (hash-ref (core-scope-bindings scp) id (unbound))]
    [(scope parent bindings)
     (define (resolve-in-parent)
       (scope-resolve (scope-parent scp) id))
     (hash-ref (scope-bindings scp) id resolve-in-parent)]
    [(disjoin mark1 scope1 mark2 scope2)
     (cond
       [(top-mark=? id mark1) (scope-resolve scope1 (drop-top-mark id))]
       [(top-mark=? id mark2) (scope-resolve scope2 (drop-top-mark id))]
       [else (error 'scope-resolve "malformed environment")])]))

;; scope-bind! : Scope Identifier Binding -> Void
;; Adds a binding to a scope. Errors if the name is already bound
;; in this scope (not counting parent scopes).
(define (scope-bind! scp id bnd)
  (match scp
    [(core-scope _)
     (error 'scope-bind "cannot bind in core scope")]
    [(scope parent bindings)
     (when (hash-has-key? bindings id)
       (error 'scope-bind! "name already bound: ~a" id))
     (hash-set! bindings id bnd)]
    [(disjoin mark1 scope1 mark2 scope2)
     (cond
       ;; this happens if we're binding a macro-introduced definition.
       ;; we need to drop the mark when we bind so this definition can be used
       ;; in other expressions that aren't marked.
       ;; since we actually bind to one of the parents of the disjoin after popping,
       ;; this is the "pushing up" of the binding through the disjoin
       ;; so we don't have to follow u/d edges down.
       [(top-mark=? id mark1) (scope-bind! scope1 (drop-top-mark id) bnd)]
       [(top-mark=? id mark2) (scope-bind! scope2 (drop-top-mark id) bnd)]
       [else (error 'scope-bind "malformed environment")])]))

;; initial-scope : core-scope
;; The root scope containing bindings for all core keywords.
;; Each keyword symbol is bound to itself.
(define initial-scope
  (core-scope
   (for/fold ([acc (hash)])
             ([sym keywords])
     (hash-set acc (identifier sym '()) sym))))

;; ============================================================
;; Identifier Operations
;; ============================================================

;; mark-id : Identifier Mark -> Identifier
;; Adds a mark to the top of an identifier's mark stack.
(define (mark-id id mark)
  (match id
    [(identifier sym marks)
     (identifier sym (cons mark marks))]))

;; mark-syntax : Syntax Mark -> Syntax
;; Recursively marks all identifiers in a syntax object.
(define (mark-syntax stx mark)
  (match stx
    [(cons a d)
     (cons (mark-syntax a mark) (mark-syntax d mark))]
    [(? identifier? id)
     (mark-id id mark)]
    [_ stx]))

;; top-mark=? : Identifier Mark -> Boolean
;; Returns #t if the identifier's top mark equals the given mark.
(define (top-mark=? id mark)
  (match id
    [(identifier sym (cons (== mark) _))
     #t]
    [_ #f]))

;; drop-top-mark : Identifier -> Identifier
;; Removes the top mark from an identifier's mark stack.
;; Precondition: The identifier has at least one mark.
(define (drop-top-mark id)
  (match id
    [(identifier sym (cons top-mark marks-rest))
     (identifier sym marks-rest)]))

;; bound-identifier=? : Identifier Identifier -> Boolean
;; Returns #t if two identifiers have the same symbol and marks.
;; This is the "same binding site" notion of equality.
(define (bound-identifier=? id1 id2)
  (and (eq? (identifier-symbol id1) (identifier-symbol id2))
       (equal? (identifier-marks id1) (identifier-marks id2))))

;; fresh-def-mark : -> Mark
;; Creates a fresh mark for definition-site identifiers.
(define (fresh-def-mark) (gensym 'd))

;; fresh-use-mark : -> Mark
;; Creates a fresh mark for use-site identifiers.
(define (fresh-use-mark) (gensym 'u))

;; ============================================================
;; S-Expression to Syntax Conversion
;; ============================================================

;; sexpr->syntax : SExpression -> Syntax
;; Converts a plain s-expression to syntax by wrapping symbols as identifiers.
(define (sexpr->syntax s)
  (match s
    [(cons a d)
     (cons (sexpr->syntax a) (sexpr->syntax d))]
    ['_ '_]
    [(? symbol? s) (symbol->id s)]
    [_ s]))

;; symbol->id : Symbol -> Identifier
;; Creates an identifier from a symbol with an empty mark list.
(define (symbol->id s)
  (identifier s (list)))

;; ============================================================
;; Gensym
;; ============================================================

;; gensym-ctr : (Parameterof (or #f Natural))
;; Counter for generating unique symbols. #f when not in expansion.
(define gensym-ctr (make-parameter #f))

;; gensym : Symbol -> Symbol
;; Generates a fresh symbol by appending a unique number to the given symbol.
(define (gensym x)
  (define ctr (gensym-ctr))
  (gensym-ctr (+ ctr 1))
  (string->symbol (format "~a~a" (symbol->string x) ctr)))

;; ============================================================
;; Pattern Environments
;; ============================================================

;; ext-env* : PatternEnv [Listof Identifier] [Listof Syntax] -> PatternEnv
;; Extends a pattern environment by binding each pattern variable to
;; its corresponding matched syntax.
(define (ext-env* penv pvar* ustx*)
  (for/fold ([penv penv])
            ([ustx ustx*] [pvar pvar*])
    (hash-set penv pvar ustx)))

;; ============================================================
;; Tests
;; ============================================================

;; The example from Fig. 17.
(module+ test
  (require rackunit)
  (check-equal?
   (expand
    '(block
      (define-syntax def-m
        (syntax-rules ()
          [(_ m given-x)
           (begin
             (define x 1)
             (define-syntax m
               (syntax-rules ()
                 [(_)
                  (begin
                    (define given-x 2)
                    (#%expression x))])))]))
      (def-m m x)
      (m)))
   '(block
     (begin)
     (begin
       (define x2 1)
       (begin))
     (begin
       (define x5 2)
       (#%expression x2)))))
