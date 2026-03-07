#lang racket

;; LSP-enabled macro expander
;;
;; This module implements a hygienic macro expander with LSP support.
;; It uses scope graphs for hygiene and tracks binding/reference
;; relationships for goto-definition, find-references, and autocomplete.
;;
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

(provide (all-defined-out))
(require "stx.rkt")
(require "stx-quote.rkt")
(require "reader.rkt")
(require racket/hash)

;; ============================================================
;; Data Definitions
;; ============================================================

;; A Mark is a Symbol created by gensym.
;; Marks distinguish identifiers introduced at different macro expansion sites,
;; and use vs introduced syntax.

;; An IdentifierKey is a (identifier-key Symbol [Listof Mark])
(struct identifier-key [symbol marks] #:transparent)
;; symbol : Symbol - the underlying symbol name
;; marks : [Listof Mark] - hygiene marks
;; Used as keys in scope bindings. Only symbol and marks matter for
;; binding resolution - id and span are irrelevant.

;; identifier->key : Identifier -> IdentifierKey
;; Extracts the symbol and marks from an identifier for use as a hash key.
(define (identifier->key id)
  (identifier-key (identifier-symbol id) (identifier-marks id)))

;; A PatternEnv is a [HashOf IdentifierKey Stx]
;; Maps each pattern variable's key to the syntax it matched at the use site.

;; A Binding is one of:
;; - VarBinding
;; - KeywordBinding
;; - MacroBinding
;; - PatternVariableBinding
;; Represents what an identifier resolves to in a scope.

;; A VarBinding is a (var-binding Identifier Symbol)
(struct var-binding [site name] #:transparent)
;; site : Identifier - the binding site identifier (has span for LSP)
;; name : Symbol - the gensym'd variable name for expanded output

;; A KeywordBinding is a (keyword-binding Symbol)
(struct keyword-binding [name] #:transparent)
;; name : Symbol - the keyword name ('let, 'define, etc.)
;; Keywords don't have surface binding sites.

;; A MacroBinding is a (macro-binding Identifier Syntax Scope)
(struct macro-binding [site macrot scp] #:transparent)
;; site : Identifier - the binding site identifier (has span for LSP)
;; macrot : Syntax - the syntax-rules transformer expression
;; scp : Scope - definition-site scope

;; A PatternVariableBinding is a (pattern-variable-binding Identifier)
(struct pattern-variable-binding [site] #:transparent)
;; site : Identifier - the pattern variable identifier in the pattern (has span for LSP)

;; ------------------------------------------------------------
;; Scopes (Scope Graph Vertices)
;; ------------------------------------------------------------

;; A Scope is one of:
;; - core-scope
;; - scope
;; - disjoin

;; A core-scope is the root of the scope graph containing core keywords.
(struct core-scope [bindings] #:transparent)
;; bindings : [HashOf IdentifierKey Binding]

;; A scope is a regular scope vertex with a parent edge and local bindings.
(struct scope [parent bindings] #:transparent)
;; parent : Scope
;; bindings : [MutableHashOf IdentifierKey Binding]

;; A disjoin is a scope vertex with one marked def-site edge and one unmarked use-site edge.
(struct disjoin [def-mark def-scp use-scp] #:transparent)
;; def-mark : Mark - definition-site mark
;; def-scp : Scope - definition-site scope (for macro-introduced identifiers)
;; use-scp : Scope - use-site scope (for pattern variable substitutions, no mark)

;; with-stx-error-handling : catches raised stx-error? and returns it
;; Use this instead of (with-handlers ([exn:fail? ...])) so unexpected errors are not silently swallowed.
(define-syntax-rule (with-stx-error-handling body ...)
  (with-handlers ([stx-error? (lambda (err) err)])
    body ...))

;; stx-error? -> void?
(define (raise-and-record-stx-error err)
  (record-stx-error! err)
  (raise err))

;; stx-error? -> void?
(define (record-stx-error! err)
  (when (current-expander-state)
    (define errs (expander-state-stx-errors (current-expander-state)))
    (set-add! errs err)))

;; ------------------------------------------------------------
;; LSP State
;; ------------------------------------------------------------

;; An ExpanderState is a (expander-state Hash Hash Hash)
(struct expander-state [span->stx resolutions references stx-errors] #:transparent)
;; span->stx : [MutableHashOf Span Stx] - maps surface span to its Stx object
;; resolutions : [MutableHashOf Span [Listof Resolution]] - maps ref span to resolutions
;; references : [MutableHashOf Span [Listof Span]] - maps def span to ref spans
;; stx-errors : [MutableSetOf stx-error?]

;; A Resolution is a (resolution Binding-or-#f Stx Scope)
(struct resolution [binding ref-stx scp] #:transparent)
;; binding : (or/c Binding #f) - the resolved binding, or #f if unbound
;; ref-stx : Stx - the reference syntax object
;; scp : Scope - the scope where resolution occurred

;; current-expander-state : (Parameter (or/c ExpanderState #f))
(define current-expander-state (make-parameter #f))

;; ------------------------------------------------------------
;; Expanded Output
;; ------------------------------------------------------------

;; An XSExpr (Expanded S-Expression) is the output of the expander.
;; It is a plain Racket s-expression tree, NOT a Stx.
;;
;; An XSExpr is one of:
;; - Number
;; - Symbol
;; - StxError
;; - (list 'block XDef ...)
;; - (list 'let (list (list Symbol XSExpr)) XSExpr)
;; - (list '#%expression XSExpr)
;;
;; An XDef (Expanded Definition) is one of:
;; - StxError
;; - (list 'define Symbol XSExpr)
;; - (list 'begin XDef ...)
;; - (list '#%expression XSExpr)

;; A Pass1Def is the intermediate representation between pass 1 and pass 2
;; of definition expansion. It is a plain Racket s-expression with embedded
;; Stx values for not-yet-expanded expressions.
;;
;; A Pass1Def is one of:
;; - StxError
;; - (list 'define Symbol Stx)          ; var-name + unexpanded body
;; - (list 'begin Pass1Def ...)
;; - (list '#%expression Stx)           ; unexpanded expression
;; - (with-disjoin Pass1Def Scope)      ; from macro expansion

;; An ExpanderResult is a (expander-result Stx XSExpr (Listof StxError) ExpanderState)
(struct expander-result [surface expanded errors state] #:transparent)
;; surface : Stx - the original surface syntax
;; expanded : XSExpr - the fully expanded s-expression
;; errors : (Listof StxError) - syntax errors found during expansion
;; state : ExpanderState - the final expander state with populated tables

;; ------------------------------------------------------------
;; Pass 1/2 Intermediate Representation
;; ------------------------------------------------------------

;; A with-disjoin is used between pass 1 and pass 2 of definition expansion.
(struct with-disjoin [stx scp] #:transparent)
;; stx : Pass1Def - partially expanded definition from pass 1
;; scp : Scope - disjoin scope to use in pass 2

;; ============================================================
;; Keywords and Initial Scope
;; ============================================================

;; keywords : [Listof Symbol]
(define keywords '(let let-syntax syntax-rules define
                    define-syntax block begin #%expression))

;; initial-scope : core-scope
;; The root scope containing bindings for all core keywords.
(define initial-scope
  (core-scope
   (for/fold ([acc (hash)])
             ([sym keywords])
     (hash-set acc (identifier-key sym '()) (keyword-binding sym)))))

;; ============================================================
;; Entry Points
;; ============================================================

;; analyze! : Stx -> ExpanderResult
;; Main entry point. Initializes state, records surface syntax, expands, returns result.
(define (analyze! surface-stx)
  (define state (make-expander-state))
  (parameterize ([current-expander-state state]
                 [gensym-ctr 0])
    (record-all-stx! surface-stx)
    (define expanded (expand-expr surface-stx initial-scope))
    (define errors (for/list ([err (expander-state-stx-errors state)]) err))
    (expander-result surface-stx expanded errors state)))

;; expand : SExpression -> SExpression
;; Legacy entry point for compatibility with existing tests.
;; Converts s-expression to syntax, expands, and returns expanded s-expression.
(define (expand e)
  (parameterize ([gensym-ctr 0])
    (syntax->sexpr (expand-expr (sexpr->syntax e) initial-scope))))

;; ============================================================
;; Expression Expansion
;; ============================================================

;; expand-expr : Stx Scope -> XSExpr
;; Expands an expression in the given scope, producing an expanded s-expression.
(define (expand-expr expr scp)
  (match expr
    [(app stx-e (? number? n)) n]
    [(? identifier? id)
     (define binding (scope-resolve scp id))
     (cond
       [(var-binding? binding) (var-binding-name binding)]
       [(keyword-binding? binding) (identifier-symbol id)]
       [(stx-error? binding) binding]
       [else (stx-error 'expand-expr "unexpected binding type" expr #f)])]
    [(stx-quote (,head-stx . ,args))
     #:when (identifier? head-stx)
     (define binding (scope-resolve scp head-stx))
     (cond
       ;; block
       [(and (keyword-binding? binding)
             (eq? 'block (keyword-binding-name binding)))
        (match-define (stx-quote (,_ ,defs ...)) expr)
        (define scp^ (new-scope scp))
        (define defs^ (expand-defs-pass1 defs scp^))
        (define defs^^ (expand-defs-pass2 defs^ scp^))
        `(block . ,defs^^)]
       ;; let
       [(and (keyword-binding? binding)
             (eq? 'let (keyword-binding-name binding)))
        (with-stx-error-handling
          (match expr
            [(stx-quote (let ([,x-stx ,e-stx]) ,body))
             (unless (identifier? x-stx)
               (raise-and-record-stx-error (stx-error 'let "bad syntax" expr x-stx)))
             (define e^ (expand-expr e-stx scp))
             (define scp^ (new-scope scp))
             (define x-name (gensym (identifier-symbol x-stx)))
             (define x-binding (var-binding x-stx x-name))
             (scope-bind! scp^ x-stx x-binding)
             (define b^ (expand-expr body scp^))
             `(let ([,x-name ,e^]) ,b^)]
            [_ (raise-and-record-stx-error (stx-error 'let "bad syntax" expr #f))]))]
       ;; let-syntax
       [(and (keyword-binding? binding)
             (eq? 'let-syntax (keyword-binding-name binding)))
        (with-stx-error-handling
          (match expr
            [(stx-quote (let-syntax ([,mname-stx ,macrot-stx]) ,body))
             (unless (identifier? mname-stx)
               (raise-and-record-stx-error (stx-error 'let-syntax "bad syntax" expr mname-stx)))
             (define scp^ (new-scope scp))
             (define m-binding (macro-binding mname-stx macrot-stx scp))
             (scope-bind! scp^ mname-stx m-binding)
             (record-all-pvar-resolutions-for-macrot! macrot-stx scp)
             (expand-expr body scp^)]
            [_ (raise-and-record-stx-error (stx-error 'let-syntax "bad syntax" expr #f))]))]
       ;; macro application
       [(macro-binding? binding)
        (with-stx-error-handling
          (define-values (marked-stx disjoined-scp) (expand-macro head-stx expr scp))
          (expand-expr marked-stx disjoined-scp))]
       ;; unbound in head position - pass through the stx-error from scope-resolve
       [(stx-error? binding)
        (for ([arg args])
          (when (identifier? arg)
            (scope-resolve scp arg)))
        binding]
       ;; variable in head position - not callable
       [(var-binding? binding)
        (for ([arg args])
          (when (identifier? arg)
            (scope-resolve scp arg)))
        (stx-error (identifier-symbol head-stx) "not a procedure or syntax" expr head-stx)]
       [else (stx-error #f "unexpected form" expr head-stx)])]
    ;; Non-identifier in head position
    [(stx-quote (,head-stx . ,_))
     (stx-error #f "not a procedure or syntax" expr head-stx)]))

;; ============================================================
;; Definition Expansion (Two-Pass)
;; ============================================================

;; Definition contexts (like block) use two-pass expansion:
;; Pass 1: Discover all bindings (variables and macros) and expand macros.
;; Pass 2: Expand all expressions now that all bindings are known.
;; This allows forward references within a definition context.

;; expand-defs-pass1 : [Listof Stx] Scope -> [Listof Pass1Def]
;; First pass over a list of definitions.
(define (expand-defs-pass1 defs scp)
  (for/list ([def defs])
    (expand-def-pass1 def scp)))

;; expand-defs-pass2 : [Listof Pass1Def] Scope -> [Listof XDef]
;; Second pass over a list of definitions.
(define (expand-defs-pass2 defs scp)
  (for/list ([def defs])
    (expand-def-pass2 def scp)))

;; expand-def-pass1 : Stx Scope -> Pass1Def
;; First pass of definition expansion: discovers bindings and expands macros.
(define (expand-def-pass1 def scp)
  (match def
    [(stx-quote (,head-stx . ,_))
     #:when (identifier? head-stx)
     (define binding (scope-resolve scp head-stx))
     (cond
       ;; define
       [(and (keyword-binding? binding)
             (eq? 'define (keyword-binding-name binding)))
        (with-stx-error-handling
          (match def
            [(stx-quote (define ,var-stx ,expr-stx))
             (unless (identifier? var-stx)
               (raise-and-record-stx-error (stx-error 'define "bad syntax" def var-stx)))
             (define var-name (gensym (identifier-symbol var-stx)))
             (define var-bnd (var-binding var-stx var-name))
             (scope-bind! scp var-stx var-bnd)
             `(define ,var-name ,expr-stx)]
            [_ (raise-and-record-stx-error (stx-error 'define "bad syntax" def #f))]))]
       ;; define-syntax
       [(and (keyword-binding? binding)
             (eq? 'define-syntax (keyword-binding-name binding)))
        (with-stx-error-handling
          (match def
            [(stx-quote (define-syntax ,var-stx ,macrot-stx))
             (unless (identifier? var-stx)
               (raise-and-record-stx-error (stx-error 'define-syntax "bad syntax" def var-stx)))
             (define m-binding (macro-binding var-stx macrot-stx scp))
             (scope-bind! scp var-stx m-binding)
             (record-all-pvar-resolutions-for-macrot! macrot-stx scp)
             `(begin)]
            [_ (raise-and-record-stx-error (stx-error 'define-syntax "bad syntax" def #f))]))]
       ;; begin
       [(and (keyword-binding? binding)
             (eq? 'begin (keyword-binding-name binding)))
        (define defs (stx-cdr def))
        (define defs^ (expand-defs-pass1 defs scp))
        `(begin . ,defs^)]
       ;; #%expression
       [(and (keyword-binding? binding)
             (eq? '#%expression (keyword-binding-name binding)))
        (with-stx-error-handling
          (match def
            [(stx-quote (#%expression ,expr-stx))
             `(#%expression ,expr-stx)]
            [_ (raise-and-record-stx-error (stx-error '#%expression "bad syntax" def #f))]))]
       ;; macro application
       [(macro-binding? binding)
        (with-stx-error-handling
          (define-values (marked-stx disjoined-scp) (expand-macro head-stx def scp))
          (with-disjoin (expand-def-pass1 marked-stx disjoined-scp) disjoined-scp))]
       ;; unbound in head position - pass through the stx-error from scope-resolve
       [(stx-error? binding) binding]
       ;; variable in head position - not callable
       [(var-binding? binding)
        (stx-error head-stx "not a procedure or syntax" def head-stx)]
       [else (stx-error #f "unexpected form" def head-stx)])]
    ;; bare non-list (e.g. a bare identifier) - not a valid definition form
    [_ (stx-error #f "not a definition form" def #f)]))

;; expand-def-pass2 : Pass1Def Scope -> XDef
;; Second pass of definition expansion: expands all expressions.
;; The scp argument is the default scope; with-disjoin wrappers
;; override it with their stored disjoin scope.
(define (expand-def-pass2 def scp)
  (match def
    [(? stx-error?) def]  ; pass through errors from pass 1
    [`(define ,var ,expr)
     `(define ,var ,(expand-expr expr scp))]
    [`(begin ,defs ...)
     (define defs^ (expand-defs-pass2 defs scp))
     `(begin . ,defs^)]
    [`(#%expression ,expr)
     `(#%expression ,(expand-expr expr scp))]
    [(with-disjoin syn scp)
     (expand-def-pass2 syn scp)]))

;; ============================================================
;; Macro Expansion
;; ============================================================

;; expand-macro : Identifier Syntax Scope -> (values Syntax Scope)
;; Expands a macro application.
;; Returns the instantiated template and a disjoin scope for continuing expansion.
(define (expand-macro mname expr use-scp)
  (define who (identifier-symbol mname))
  (define binding (scope-resolve use-scp mname))
  (match-define (macro-binding _ macrot def-scp) binding)
  (define-values (penv tmpl)
    (select-syntax-rule who macrot expr))
  (define def-mark (fresh-def-mark))
  (define expanded-tmpl (expand-template tmpl penv def-mark))
  (define introduced-defn-scp (new-scope def-scp))
  (define disjoined-scp (disjoin def-mark introduced-defn-scp use-scp))
  (values expanded-tmpl disjoined-scp))

;; expand-template : Syntax PatternEnv Mark -> Syntax
;; Instantiates a template: substitutes pattern variable references with
;; use-site syntax from penv; marks all other identifiers with def-mark.
(define (expand-template tmpl penv def-mark)
  (match tmpl
    [(? identifier? id)
     (if (hash-has-key? penv (identifier->key id))
         (hash-ref penv (identifier->key id))   ; use-site syntax, keep as-is
         (mark-id id def-mark))]           ; macro-introduced, mark it
    [(stx (? list? elems) spn marks)
     (stx (map (lambda (t) (expand-template t penv def-mark)) elems) spn marks)]
    [(stx (cons a d) spn marks)
     (stx (cons (expand-template a penv def-mark)
                (expand-template d penv def-mark))
          spn marks)]
    [_ tmpl]))  ; numbers, booleans, etc. pass through

;; ============================================================
;; Pattern Variable LSP Resolution
;; ============================================================

;; record-all-pvar-resolutions-for-macrot! : Syntax Scope -> Void
;; Eagerly records LSP pvar resolutions for every clause in a syntax-rules transformer.
;; Called at let-syntax/define-syntax expansion time so that goto-definition,
;; find-references, and autocomplete work on pattern variables even if the macro
;; is never applied.
(define (record-all-pvar-resolutions-for-macrot! macrot def-scp)
  (match macrot
      [(stx-quote (,_syntax-rules (,literal-ids ...) ,clauses ...))
       (define is-literal? (make-is-datum-literal? literal-ids))
       (for ([clause clauses])
         (match clause
           [(stx-quote [,pat ,tmpl])
            (define pvar-scp (build-pvar-scope pat is-literal? def-scp))
            (record-pvar-resolutions! tmpl pvar-scp)]))]))

;; build-pvar-scope : Pattern (Id -> Bool) Scope -> Scope
;; Builds a scope containing a pattern-variable-binding for each pvar in the pattern.
;; Also records each pvar as a binding site so goto-definition works even if the pvar
;; is never referenced in the template.
;; The scope's parent is def-scp so autocomplete traversal includes definition-site names.
;; The wildcard _ is excluded since it is never referenced in templates.
(define (build-pvar-scope pat is-literal? def-scp)
  (define pvar-scp (new-scope def-scp))
  (let loop ([p pat])
    (match p
      [(? identifier? id)
       (unless (or (is-literal? id) (eq? (identifier-symbol id) '_))
         (scope-bind! pvar-scp id (pattern-variable-binding id)))]
      [(stx-quote (,a . ,d))
       (loop a)
       (loop d)]
      [_ (void)]))
  pvar-scp)

;; record-pvar-resolutions! : Syntax Scope -> Void
;; Walks a template and records LSP resolutions for pattern variable references.
;; For each identifier that resolves to a pattern-variable-binding in pvar-scp,
;; records the resolution so goto-definition and find-references work on template uses.
;; Template-introduced identifiers that don't resolve to pvars are silently skipped.
(define (record-pvar-resolutions! tmpl pvar-scp)
  (match tmpl
    [(? identifier? id)
     (define bnd (scope-resolve-internal pvar-scp id))
     (when (pattern-variable-binding? bnd)
       (record-resolution! id bnd pvar-scp))]
    [(stx-quote (,a . ,d))
     (record-pvar-resolutions! a pvar-scp)
     (record-pvar-resolutions! d pvar-scp)]
    [_ (void)]))

;; ============================================================
;; Syntax-Rules Matching
;; ============================================================

;; select-syntax-rule : Symbol Syntax Syntax Scope Scope -> (values PatternEnv Syntax)
;; Selects the first matching clause from a syntax-rules transformer.
;; Returns the pattern environment and the template to instantiate.
(define (select-syntax-rule who macrot expr)
  ;; macrot is (syntax-rules (literal ...) clause ...)
  (match macrot
    [(stx-quote (,_syntax-rules (,literal-ids ...) ,clauses ...))
     (define is-datum-literal? (make-is-datum-literal? literal-ids))
     (try-clauses who clauses expr is-datum-literal?)]))

;; try-clauses : Symbol [Listof Clause] Syntax (Id -> Bool) -> (values PatternEnv Syntax)
;; Tries each clause in order until one matches.
;; A Clause is (list Pattern Template).
(define (try-clauses who clauses expr is-datum-literal?)
  (match clauses
    [(cons clause rest)
     (match clause
       [(stx-quote [,pat ,tmpl])
        (define maybe-penv (match-top-pattern pat expr is-datum-literal?))
        (if maybe-penv
            (values maybe-penv tmpl)
            (try-clauses who rest expr is-datum-literal?))])]
    ['() (raise-and-record-stx-error (stx-error who "no pattern matched" expr #f))]))

;; match-top-pattern : Pattern Syntax (Id -> Bool) -> (or PatternEnv #f)
;; Matches a top-level pattern against syntax.
;; The car of both pattern and syntax is the macro name (ignored per syntax-rules semantics).
;; Returns a PatternEnv on success, #f on failure.
(define (match-top-pattern pat expr is-datum-literal?)
  (match* (pat expr)
    [((stx-quote (,_ . ,pd)) (stx-quote (,_ . ,ed)))
     (match-pattern pd ed is-datum-literal?)]
    [(_ _) #f]))

;; match-pattern-list : [Listof Pattern] [Listof Syntax] (Identifier -> Boolean) -> (or PatternEnv #f)
;; Matches a list of patterns against a list of syntax elements.
(define (match-pattern-list pats exprs is-datum-literal?)
  (cond
    [(and (null? pats) (null? exprs)) (hash)]
    [(or (null? pats) (null? exprs)) #f]
    [else
     (define resa (match-pattern (car pats) (car exprs) is-datum-literal?))
     (and resa
          (let ([resd (match-pattern-list (cdr pats) (cdr exprs) is-datum-literal?)])
            (and resd (hash-union resa resd))))]))

;; match-pattern : Pattern Syntax (Id -> Bool) -> (or PatternEnv #f)
;; Matches a pattern against syntax.
;; Returns a PatternEnv mapping pattern variables to matched syntax on success,
;; or #f if the pattern doesn't match.
(define (match-pattern pat expr is-datum-literal?)
  (match* (pat expr)
    [((? identifier? lit) (? identifier? target-id))
     #:when (is-datum-literal? lit)
     (and (equal? (stx->datum lit) (stx->datum target-id))
          (hash))]
    [((? identifier? pvar) syn)
     #:when (not (is-datum-literal? pvar))
     (hash (identifier->key pvar) syn)]
    [((stx-quote (,pa . ,pd)) (stx-quote (,ea . ,ed)))
     (=> fail)
     (define penv-a (match-pattern pa ea is-datum-literal?))
     (unless penv-a (fail))
     (define penv-d (match-pattern pd ed is-datum-literal?))
     (unless penv-d (fail))
     (hash-union penv-a penv-d
                 #:combine (lambda (ea ed)
                             (if (syntax-same-for-binding? ea ed)
                                 ea
                                 (fail))))]
    [(_ _)
    (and (equal? (stx->datum pat) (stx->datum expr))
         (hash))]))

;; make-is-literal? : [Listof Identifier] -> (Identifier -> Boolean)
;; Creates a predicate that checks if an identifier is a literal
;; (using bound-identifier=? comparison).
(define (make-is-datum-literal? literal-ids)
  (lambda (id)
    (memf (lambda (x) (eq? (identifier-symbol x) (identifier-symbol id))) literal-ids)))

;; ============================================================
;; Scope Operations
;; ============================================================

;; new-scope : Scope -> Scope
;; Creates a new scope vertex with the given parent and empty bindings.
(define (new-scope parent)
  (scope parent (make-hash)))

;; scope-resolve : Scope Identifier -> (or Binding stx-error)
;; Resolves an identifier by traversing parent edges in the scope graph.
;; Records the resolution in the LSP tables.
;; Returns a stx-error if the identifier is unbound.
(define (scope-resolve scp id)
  (define binding (scope-resolve-internal scp id))
  ;; Record the resolution for LSP
  (record-resolution! id (if (stx-error? binding) #f binding) scp)
  (when (stx-error? binding)
    (record-stx-error! binding))
  binding)

;; scope-resolve-internal : Scope Identifier -> (or Binding stx-error)
;; Internal resolution without recording (to avoid double-recording).
;; Returns a stx-error if the identifier is unbound.
(define (scope-resolve-internal scp id)
  (define key (identifier->key id))
  (match scp
    [(core-scope core-bindings)
     (hash-ref core-bindings key (lambda () (stx-error (identifier-symbol id) "unbound identifier" id #f)))]
    [(scope parent bindings)
     (hash-ref bindings key (lambda () (scope-resolve-internal parent id)))]
    [(disjoin def-mark def-scp use-scp)
     (cond
       [(top-mark=? id def-mark) (scope-resolve-internal def-scp (drop-top-mark id))]
       [else (scope-resolve-internal use-scp id)])]))

;; scope-bind! : Scope Identifier Binding -> Void
;; Adds a binding to a scope using IdentifierKey.
;; Also records the binding site as a self-reference for LSP.
;; Records the resolution with the PARENT scope so that autocomplete
;; at the binding site shows what was in scope before this binding.
(define (scope-bind! scp id bnd)
  (define key (identifier->key id))
  (match scp
    [(core-scope _)
     (error 'scope-bind "cannot bind in core scope")]
    [(scope parent bindings)
     (when (hash-has-key? bindings key)
       (error 'scope-bind! "name already bound: ~a" id))
     (hash-set! bindings key bnd)
     (record-binding-site! id)]
    [(disjoin def-mark def-scp use-scp)
     (if (top-mark=? id def-mark)
         (scope-bind! def-scp (drop-top-mark id) bnd)
         (scope-bind! use-scp id bnd))]))

;; scope-snapshot : Scope -> Scope
;; Creates a deep copy of a scope, capturing the current bindings as an immutable snapshot.
;; Prevents future scope-bind! mutations from affecting this copy.
(define (scope-snapshot scp)
  (match scp
    [(core-scope _) scp]  ; immutable hash, safe to share
    [(scope parent bindings)
     (scope (scope-snapshot parent) (hash-copy bindings))]
    [(disjoin def-mark def-scp use-scp)
     (disjoin def-mark (scope-snapshot def-scp) (scope-snapshot use-scp))]))

;; ============================================================
;; Identifier Operations
;; ============================================================

;; mark-syntax : Identifier Mark -> Identifier
;; Mark an identifier
(define (mark-id id mark)
  (match id
    [(stx x spn marks)
     (stx x spn (cons mark marks))]))

;; top-mark=? : Identifier Mark -> Boolean
;; Returns #t if the identifier's top mark equals the given mark.
(define (top-mark=? id mark)
  (match id
    [(app stx-marks (cons (== mark) _)) #t]
    [_ #f]))

;; drop-top-mark : Identifier -> Identifier
;; Removes the top mark from an identifier's mark stack.
;; Precondition: The identifier has at least one mark.
(define (drop-top-mark id)
  (match id
    [(stx e spn (cons _ marks-rest))
     (stx e spn marks-rest)]))

;; Syntax Syntax -> Boolean
;; Are the two syntaxes the same up to datums and marks?
(define (syntax-same-for-binding? a b)
  (match* (a b)
    [((stx-quote (,aa . ,ad)) (stx-quote (,ba . ,bd)))
     (and (syntax-same-for-binding? aa ba)
          (syntax-same-for-binding? ad bd))]
    [((stx-quote ()) (stx-quote ()))
     #t]
    [((? identifier?) (? identifier?))
     (bound-identifier=? a b)]
    [(_ _)
     (equal? (stx->datum a) (stx->datum b))]))

;; bound-identifier=? : Identifier Identifier -> Boolean
;; Returns #t if two identifiers have the same symbol and marks.
;; This is the "same binding site" notion of equality.
(define (bound-identifier=? id1 id2)
  (and (eq? (identifier-symbol id1) (identifier-symbol id2))
       (equal? (identifier-marks id1) (identifier-marks id2))))

;; fresh-def-mark : -> Mark
;; Creates a fresh mark for definition-site identifiers.
(define (fresh-def-mark) (gensym 'd))

;; ============================================================
;; Gensym
;; ============================================================

;; gensym-ctr : (Parameter (or #f Natural))
(define gensym-ctr (make-parameter #f))

;; gensym : Symbol -> Symbol
;; Generates a fresh symbol by appending a unique number to the given symbol.
(define (gensym x)
  (define ctr (gensym-ctr))
  (gensym-ctr (+ ctr 1))
  (string->symbol (format "~a~a" (symbol->string x) ctr)))

;; ============================================================
;; LSP Recording Functions
;; ============================================================

;; make-expander-state : -> ExpanderState
;; Creates a fresh expander state with empty tables.
(define (make-expander-state)
  (expander-state (make-hash) (make-hash) (make-hash) (mutable-set)))

;; hash-cons! : MutableHash Key Value -> Void
;; Appends a value to the list stored at key (multi-valued hash).
(define (hash-cons! ht key val)
  (hash-set! ht key (cons val (hash-ref ht key '()))))

;; record-stx! : Stx -> Void
;; Records a surface syntax node in the span->stx table.
(define (record-stx! stx)
  (define spn (stx-span stx))
  (define state (current-expander-state))
  (when (and spn state)
    (hash-set! (expander-state-span->stx state) spn stx)))

;; record-resolution! : Stx Binding-or-#f Scope -> Void
;; Records a resolution in the tables.
;; - Adds to resolutions table for goto-definition and autocomplete
;; - Adds to references table for find-references (if binding has surface site)
(define (record-resolution! ref-stx binding scp)
  (define ref-spn (stx-span ref-stx))
  (define state (current-expander-state))
  ;; TODO if state is #f, we should error instead of silently failing
  (when (and ref-spn state)
    ;; Record the resolution with a snapshot of the scope to prevent future
    ;; scope-bind! mutations from affecting autocomplete results (issue #45).
    (hash-cons! (expander-state-resolutions state) ref-spn
                (resolution binding ref-stx (scope-snapshot scp)))
    ;; If binding has a surface site, record in references table
    (define site (and binding (binding-site binding)))
    (define def-spn (and site (stx-span site)))
    (when def-spn
      (hash-cons! (expander-state-references state) def-spn ref-spn))))

;; binding-site : Binding -> (or/c Identifier #f)
;; Extract the binding site identifier from a binding.
(define (binding-site bnd)
  (match bnd
    [(var-binding site _) site]
    [(macro-binding site _ _) site]
    [(pattern-variable-binding site) site]
    [(keyword-binding _) #f]))

;; record-binding-site! : Identifier -> Void
;; Ensures the binding site's span is registered in the references table.
;; This allows goto-definition to recognize the identifier as a binder
;; even when it has no references.
(define (record-binding-site! id)
  (define spn (stx-span id))
  (define state (current-expander-state))
  (when (and spn state)
    (define references (expander-state-references state))
    (unless (hash-has-key? references spn)
      (hash-set! references spn '()))))

;; record-all-stx! : Syntax -> Void
;; Records all surface syntax nodes in the span->stx table.
(define (record-all-stx! root-stx)
  (let loop ([syn root-stx])
    (match syn
      [(stx e spn _marks)
       (when spn (record-stx! syn))
       (cond
         [(list? e) (for-each loop e)]
         [(pair? e) (loop (car e)) (loop (cdr e))])]
      [_ (void)])))

;; ============================================================
;; Query Functions
;; ============================================================

;; get-binding-sites-of : ExpanderResult Stx -> (Listof Stx)
;; Given a reference site, returns the binding sites it resolves to.
;; Filters out #f results (core keywords have no binding site).
;; Returns empty list if the node has no span or no recorded resolutions.
(define (get-binding-sites-of result ref-stx)
  (define ref-spn (stx-span ref-stx))
  (define resolutions (expander-state-resolutions (expander-result-state result)))
  (if ref-spn
      (remove-duplicates
       (filter-map
        (lambda (res)
          (define bnd (resolution-binding res))
          (and bnd (binding-site bnd)))
        (hash-ref resolutions ref-spn '()))
       #:key stx-span)
      '()))

;; get-reference-sites-of : ExpanderResult Stx -> (Listof Stx)
;; Given a binding site OR reference site, returns all reference sites.
;; If given a binding site, returns all references to that binding.
;; If given a reference site, first finds its binding sites, then finds
;; all references to those binding sites.
;; Includes the binding site itself (binding sites are self-references).
(define (get-reference-sites-of result stx-node)
  (define spn (stx-span stx-node))
  (define state (expander-result-state result))
  (define references (expander-state-references state))
  (define span->stx (expander-state-span->stx state))
  (if spn
      ;; First check if this is a binding site (has entries in references table)
      (let ([direct-refs (hash-ref references spn '())])
        (if (not (null? direct-refs))
            ;; It's a binding site - return all references to it (deduplicated)
            (remove-duplicates
             (for/list ([ref-spn direct-refs])
               (hash-ref span->stx ref-spn))
             #:key stx-span)
            ;; It's a reference site - find binding sites, then their references
            (let* ([bsites (get-binding-sites-of result stx-node)]
                   [binding-spns (filter-map stx-span bsites)])
              (remove-duplicates
               (append-map
                (lambda (def-spn)
                  (for/list ([ref-spn (hash-ref references def-spn '())])
                    (hash-ref span->stx ref-spn)))
                binding-spns)
               #:key stx-span))))
      '()))

;; scope->names : Scope [Listof Mark] -> (Set Symbol)
;; Traverse scope graph to collect names accessible with the given marks.
;; Only returns names whose binding keys have marks matching the current marks.
;; At disjoins, drops the top mark when traversing the matching edge.
(define (scope->names scp marks)
  (match scp
    [(core-scope bindings)
     (for/seteq ([key (in-hash-keys bindings)]
                 #:when (equal? (identifier-key-marks key) marks))
       (identifier-key-symbol key))]
    [(scope parent bindings)
     (set-union
      (for/seteq ([key (in-hash-keys bindings)]
                  #:when (equal? (identifier-key-marks key) marks))
        (identifier-key-symbol key))
      (scope->names parent marks))]
    [(disjoin def-mark def-scp use-scp)
     (cond
       [(and (pair? marks) (eq? (car marks) def-mark))
        (scope->names def-scp (cdr marks))]
       [else
        (scope->names use-scp marks)])]))

;; get-names-in-scope : ExpanderResult Stx -> (Set Symbol)
;; Returns the names in scope at the given syntax node.
;; Uses the Resolution's ref-stx marks and scope to traverse the graph.
;; If the node was resolved under multiple scopes (macro duplication),
;; returns the intersection of names from all scopes.
(define (get-names-in-scope result stx-node)
  (define spn (stx-span stx-node))
  (define resolutions (expander-state-resolutions (expander-result-state result)))
  (if spn
      (let ([res-list (hash-ref resolutions spn '())])
        (if (null? res-list)
            (seteq)
            (apply set-intersect
                   (for/list ([res res-list])
                     (scope->names (resolution-scp res)
                                   (identifier-marks (resolution-ref-stx res)))))))
      (seteq)))

;; get-all-surface-binding-sites : ExpanderResult -> (Listof Stx)
;; Returns all surface binding sites in the program.
;; A binding site is a surface node that has entries in the references table.
(define (get-all-surface-binding-sites result)
  (define state (expander-result-state result))
  (define references (expander-state-references state))
  (define span->stx (expander-state-span->stx state))
  (for/list ([def-spn (in-hash-keys references)])
    (hash-ref span->stx def-spn)))

;; ============================================================
;; Position-Based Query Functions
;; ============================================================

;; find-node-at-position : ExpanderResult Loc -> (or/c Stx #f)
;; Given an LSP position (line+column), find the innermost surface syntax node
;; at that position. Searches the surface syntax tree.
(define (find-node-at-position result pos)
  (define surface (expander-result-surface result))
  (find-node-at-position-in surface pos))

;; find-node-at-position-in : Stx Loc -> (or/c Stx #f)
;; Finds the innermost surface syntax node containing the given position.
(define (find-node-at-position-in root pos)
  (let loop ([syn root])
    (match syn
      [(stx e spn _marks)
       (cond
         ;; If this node has a span and contains the position, check children first
         [(and spn (span-contains? spn pos))
          (cond
            [(list? e)
             (or (ormap loop e)
                 syn)]
            [(pair? e)
             (or (loop (car e))
                 (loop (cdr e))
                 syn)]
            [else
             syn])]
         ;; No span or doesn't contain position - try children anyway
         [(list? e) (ormap loop e)]
         [(pair? e) (or (loop (car e)) (loop (cdr e)))]
         [else #f])]
      [_ #f])))

;; span-contains? : Span Loc -> Boolean
;; Returns #t if the span contains the given position.
(define (span-contains? spn pos)
  (and (loc<=? (span-start spn) pos)
       (loc<=? pos (span-end spn))))

;; loc<=? : Loc Loc -> Boolean
;; Returns #t if loc1 is before or at loc2.
(define (loc<=? loc1 loc2)
  (or (< (loc-line loc1) (loc-line loc2))
      (and (= (loc-line loc1) (loc-line loc2))
           (<= (loc-column loc1) (loc-column loc2)))))

;; loc<? : Loc Loc -> Boolean
;; Returns #t if loc1 is strictly before loc2.
(define (loc<? loc1 loc2)
  (or (< (loc-line loc1) (loc-line loc2))
      (and (= (loc-line loc1) (loc-line loc2))
           (< (loc-column loc1) (loc-column loc2)))))

;; goto-definition : ExpanderResult Loc -> (Listof Span)
;; Returns spans of binding sites for the identifier at position.
;; If the node is itself a binding site, includes its own span
;; (triggers VS Code's "already here" fallback to show references).
(define (goto-definition result pos)
  (define node (find-node-at-position result pos))
  (cond
    [(not node) '()]
    [else
     (define resolution-sites (filter-map stx-span (get-binding-sites-of result node)))
     (define spn (stx-span node))
     (define is-binder? (and spn (hash-has-key? (expander-state-references (expander-result-state result)) spn)))
     (define self-span (and is-binder? spn))
     (if self-span
         (remove-duplicates (cons self-span resolution-sites) equal?)
         resolution-sites)]))

;; find-references : ExpanderResult Loc -> (Listof Span)
;; Returns spans of all reference sites for the node at position.
(define (find-references result pos)
  (define node (find-node-at-position result pos))
  (if node
      (filter-map stx-span (get-reference-sites-of result node))
      '()))

;; autocomplete : ExpanderResult Loc -> (Set Symbol)
;; Returns names in scope at position.
;; If position is at an identifier, uses its recorded resolution.
;; Otherwise, inserts a cursor and re-expands to find names in scope.
(define (autocomplete result pos)
  (define node (find-node-at-position result pos))
  (cond
    [(and node (identifier? node))
     (get-names-in-scope result node)]
    [else
     ;; Insert cursor and re-expand
     (define cursor (make-cursor pos))
     (define with-cursor (insert-cursor-at (expander-result-surface result) pos cursor))
     (define cursor-result (analyze! with-cursor))
     (get-names-in-scope cursor-result cursor)]))

;; make-cursor : Loc -> Stx
;; Creates a cursor identifier at the given position.
;; It's a normal surface identifier with a gensym'd name and a zero-width span.
(define (make-cursor pos)
  (define zero-span (span pos pos))
  ;; Use Racket's built-in gensym to avoid needing gensym-ctr parameter
  (stx (string->symbol (format "cursor~a" (random 1000000))) zero-span '()))

;; insert-cursor-at : Stx Loc -> Stx
;; Inserts a cursor identifier at the given position in the syntax tree.
;; Finds the appropriate list position and inserts the cursor there.
(define (insert-cursor-at root pos cursor)
  (let loop ([syn root])
    (match syn
      [(stx e spn marks)
       (cond
         ;; Empty list node - insert cursor here if position is nearby
         [(and (null? e) spn (span-contains-or-at? spn pos))
          (stx (list cursor) spn marks)]
         ;; If this is a list node and the position is inside it,
         ;; try to insert the cursor in the right place
         [(and spn (list? e) (not (null? e)) (span-contains? spn pos))
          (define new-elems (map loop e))
          (if (equal? new-elems e)
              ;; No child changed, insert cursor at the right position
              (stx (insert-cursor-in-elems e pos cursor) spn marks)
              (stx new-elems spn marks))]
         [(and (list? e) (not (null? e)))
          (define new-elems (map loop e))
          (if (equal? new-elems e) syn (stx new-elems spn marks))]
         ;; Dotted pair
         [(and spn (pair? e) (span-contains? spn pos))
          (define new-car (loop (car e)))
          (define new-cdr (loop (cdr e)))
          (stx (cons new-car new-cdr) spn marks)]
         [(pair? e)
          (define new-car (loop (car e)))
          (define new-cdr (loop (cdr e)))
          (if (and (eq? new-car (car e)) (eq? new-cdr (cdr e)))
              syn
              (stx (cons new-car new-cdr) spn marks))]
         [else syn])]
      [_ syn])))

;; span-contains-or-at? : Span Loc -> Boolean
;; Returns #t if the span contains the position or the position is at the end.
(define (span-contains-or-at? spn pos)
  (and (loc<=? (span-start spn) pos)
       (loc<=? pos (span-end spn))))

;; insert-cursor-in-elems : (Listof Stx) Loc Stx -> (Listof Stx)
;; Inserts cursor into a list of stx elements at the appropriate position.
(define (insert-cursor-in-elems elems pos cursor)
  (cond
    [(null? elems) (list cursor)]
    [else
     (define head (car elems))
     (define rest (cdr elems))
     (cond
       [(null? rest)
        ;; Last element - insert cursor after it
        (list head cursor)]
       [else
        ;; Check if cursor should go before the next element
        (define next (car rest))
        (if (and (stx? next) (stx-span next)
                 (loc<? pos (span-start (stx-span next))))
            ;; Insert cursor before next
            (cons head (cons cursor rest))
            ;; Continue searching
            (cons head (insert-cursor-in-elems rest pos cursor)))])]))

;; ============================================================
;; Tests
;; ============================================================

(module+ test
  (require rackunit)
  
  ;; The example from Fig. 17.
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
       (define x1 1)
       (begin))
     (begin
       (define x3 2)
       (#%expression x1))))

  ;; make sure dotted patterns work
  (check-match
   (expand
    '(let-syntax ([m (syntax-rules () [(m . a) (let ([a 2]) a)])])
        (m . a)))
   '(let ([a1 2]) a1))
  
  ;; dotted (a . (b)) = (a b)
  (check-match
   (expand
    '(block (#%expression . (2))))
   '(block (#%expression 2)))
  
  ;; fault-tolerant block
  (check-match
   (expand
    '(block
       (bad)
       (define x 2)))
   `(block
      ,(? stx-error?)
      (define x0 2)))

  ;; bare identifier in block - should produce stx-error, not crash
  (check-match
   (expand '(block x))
   `(block ,(? stx-error?)))
  
  ;; datum literals
  (check-equal?
   (expand 
    '(block (define-syntax lit (syntax-rules (x y) [(lit x) 1] [(lit y) 2] [(lit z) 3]))
            (#%expression (lit x))
            (#%expression (lit y))
            (#%expression (lit something-else))))
   '(block (begin)
           (#%expression 1)
           (#%expression 2)
           (#%expression 3))))