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

;; A Binding is one of:
;; - VarBinding
;; - KeywordBinding
;; - MacroBinding
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

;; A disjoin is a scope vertex with two marked parent edges.
(struct disjoin [mark1 scope1 mark2 scope2] #:transparent)
;; mark1 : Mark - definition-site mark
;; scope1 : Scope - definition-site scope
;; mark2 : Mark - use-site mark
;; scope2 : Scope - use-site scope

;; An unbound is a sentinel value indicating that resolution failed.
;; TODO just use stx-error
(struct unbound [] #:transparent)

;; ------------------------------------------------------------
;; LSP State
;; ------------------------------------------------------------

;; An ExpanderState is a (expander-state Hash Hash Hash)
(struct expander-state [id->stx resolutions references] #:transparent)
;; id->stx : [MutableHashOf NodeId Stx] - maps surface node ID to its Stx object
;; resolutions : [MutableHashOf NodeId [Listof Resolution]] - maps ref ID to resolutions
;; references : [MutableHashOf NodeId [Listof NodeId]] - maps def ID to ref IDs

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
    (define errors (find-stx-errors expanded))
    (expander-result surface-stx expanded errors state)))

;; expand : SExpression -> SExpression
;; Legacy entry point for compatibility with existing tests.
;; Converts s-expression to syntax, expands, and returns expanded s-expression.
(define (expand e)
  (parameterize ([gensym-ctr 0]
                 [current-id 0])
    (syntax->sexpr (expand-expr (sexpr->syntax e) initial-scope))))

;; ============================================================
;; Expression Expansion
;; ============================================================

;; expand-expr : Stx Scope -> XSExpr
;; Expands an expression in the given scope, producing an expanded s-expression.
(define (expand-expr expr scp)
  (match expr
    [(stx (? number? n) _ _ _) n]
    [(? identifier? id)
     (define binding (scope-resolve scp id))
     (cond
       [(var-binding? binding) (var-binding-name binding)]
       [(keyword-binding? binding) (identifier-symbol id)]
       [(unbound? binding) (stx-error 'expand "unbound identifier" expr #f)]
       [else (stx-error 'expand-expr "unexpected binding type" expr #f)])]
    [(stx (list* head-stx _) _ _ _)
     #:when (identifier? head-stx)
     (define binding (scope-resolve scp head-stx))
     (cond
       ;; block
       [(and (keyword-binding? binding)
             (eq? 'block (keyword-binding-name binding)))
        (define def* (cdr (stx-list expr)))
        (define scp^ (new-scope scp))
        (define def*^ (expand-def*-pass1 def* scp^))
        (define def*^^ (expand-def*-pass2 def*^ scp^))
        `(block . ,def*^^)]
       ;; let
       [(and (keyword-binding? binding)
             (eq? 'let (keyword-binding-name binding)))
        (with-handlers ([exn:fail? (lambda (_e) (stx-error 'let "bad syntax" expr #f))])
          (define elems (stx-list expr))
          (define binding-clause (car (stx-list (list-ref elems 1))))
          (define body (list-ref elems 2))
          (match (stx-list binding-clause)
            [(list x-stx e-stx)
             (define e^ (expand-expr e-stx scp))
             (define scp^ (new-scope scp))
             (define x-name (gensym (identifier-symbol x-stx)))
             (define x-binding (var-binding x-stx x-name))
             (scope-bind! scp^ x-stx x-binding)
             (define b^ (expand-expr body scp^))
             `(let ([,x-name ,e^]) ,b^)]
            [_ (stx-error 'let "bad syntax" expr #f)]))]
       ;; let-syntax
       [(and (keyword-binding? binding)
             (eq? 'let-syntax (keyword-binding-name binding)))
        (with-handlers ([exn:fail? (lambda (_e) (stx-error 'let-syntax "bad syntax" expr #f))])
          (define elems (stx-list expr))
          (define binding-clause (car (stx-list (list-ref elems 1))))
          (define body (list-ref elems 2))
          (match (stx-list binding-clause)
            [(list mname-stx macrot-stx)
             (define scp^ (new-scope scp))
             (define m-binding (macro-binding mname-stx macrot-stx scp))
             (scope-bind! scp^ mname-stx m-binding)
             (expand-expr body scp^)]
            [_ (stx-error 'let-syntax "bad syntax" expr #f)]))]
       ;; macro application
       [(macro-binding? binding)
        (define who (identifier-symbol (macro-binding-site binding)))
        (with-handlers ([exn:fail? (lambda (_e) (stx-error who "bad syntax" expr #f))])
          (define-values (marked-stx disjoined-scp) (expand-macro head-stx expr scp))
          (expand-expr marked-stx disjoined-scp))]
       ;; unbound or variable in head position - syntax error
       ;; Still resolve identifier arguments for fault-tolerant LSP support
       [(or (unbound? binding) (var-binding? binding))
        (for ([arg (cdr (stx-list expr))])
          (when (identifier? arg)
            (scope-resolve scp arg)))
        (stx-error 'expand "not a procedure or syntax" expr head-stx)]
       [else (stx-error 'expand-expr "unexpected form: ~a" expr head-stx)])]
    ;; Non-identifier in head position or other forms
    [(stx (list* _ _) _ _ _)
     (stx-error 'expand "bad syntax" expr #f)]))

;; ============================================================
;; Definition Expansion (Two-Pass)
;; ============================================================

;; Definition contexts (like block) use two-pass expansion:
;; Pass 1: Discover all bindings (variables and macros) and expand macros.
;; Pass 2: Expand all expressions now that all bindings are known.
;; This allows forward references within a definition context.

;; expand-def*-pass1 : [Listof Stx] Scope -> [Listof Pass1Def]
;; First pass over a list of definitions.
(define (expand-def*-pass1 def* scp)
  (for/list ([def def*])
    (expand-def-pass1 def scp)))

;; expand-def*-pass2 : [Listof Pass1Def] Scope -> [Listof XDef]
;; Second pass over a list of definitions.
(define (expand-def*-pass2 def* scp)
  (for/list ([def def*])
    (expand-def-pass2 def scp)))

;; expand-def-pass1 : Stx Scope -> Pass1Def
;; First pass of definition expansion: discovers bindings and expands macros.
(define (expand-def-pass1 def scp)
  (match def
    [(stx (list* head-stx _) _ _ _)
     #:when (identifier? head-stx)
     (define binding (scope-resolve scp head-stx))
     (cond
       ;; define
       [(and (keyword-binding? binding)
             (eq? 'define (keyword-binding-name binding)))
        (with-handlers ([exn:fail? (lambda (_e) (stx-error 'define "bad syntax" def #f))])
          (define elems (stx-list def))
          (define var-stx (list-ref elems 1))
          (define expr-stx (list-ref elems 2))
          (define var-name (gensym (identifier-symbol var-stx)))
          (define var-bnd (var-binding var-stx var-name))
          (scope-bind! scp var-stx var-bnd)
          `(define ,var-name ,expr-stx))]
       ;; define-syntax
       [(and (keyword-binding? binding)
             (eq? 'define-syntax (keyword-binding-name binding)))
        (with-handlers ([exn:fail? (lambda (_e) (stx-error 'define-syntax "bad syntax" def #f))])
          (define elems (stx-list def))
          (define var-stx (list-ref elems 1))
          (define macrot-stx (list-ref elems 2))
          (define m-binding (macro-binding var-stx macrot-stx scp))
          (scope-bind! scp var-stx m-binding)
          `(begin))]
       ;; begin
       [(and (keyword-binding? binding)
             (eq? 'begin (keyword-binding-name binding)))
        (define def* (cdr (stx-list def)))
        (define def*^ (expand-def*-pass1 def* scp))
        `(begin . ,def*^)]
       ;; #%expression
       [(and (keyword-binding? binding)
             (eq? '#%expression (keyword-binding-name binding)))
        (with-handlers ([exn:fail? (lambda (_e) (stx-error '#%expression "bad syntax" def #f))])
          (define expr-stx (list-ref (stx-list def) 1))
          `(#%expression ,expr-stx))]
       ;; macro application
       [(macro-binding? binding)
        (define-values (marked-stx disjoined-scp) (expand-macro head-stx def scp))
        (with-disjoin (expand-def-pass1 marked-stx disjoined-scp) disjoined-scp)]
       ;; unbound or variable in head position - syntax error
       [(or (unbound? binding) (var-binding? binding))
        (stx-error 'expand "not a procedure or syntax" def head-stx)]
       [else (error 'expand-def-pass1 "unexpected form: ~a" def)])]))

;; expand-def-pass2 : Pass1Def Scope -> XDef
;; Second pass of definition expansion: expands all expressions.
;; The scp argument is the default scope; with-disjoin wrappers
;; override it with their stored disjoin scope.
(define (expand-def-pass2 def scp)
  (match def
    [(? stx-error?) def]  ; pass through errors from pass 1
    [`(define ,var ,expr)
     `(define ,var ,(expand-expr expr scp))]
    [`(begin ,def* ...)
     (define def*^ (expand-def*-pass2 def* scp))
     `(begin . ,def*^)]
    [`(#%expression ,expr)
     `(#%expression ,(expand-expr expr scp))]
    [(with-disjoin syn scp)
     (expand-def-pass2 syn scp)]))

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
  (define binding (scope-resolve use-scp mname))
  (match-define (macro-binding _ macrot def-scp) binding)
  (define-values (penv tmpl)
    (select-syntax-rule macrot expr def-scp use-scp))
  (define def-mark (fresh-def-mark))
  (define use-mark (fresh-use-mark))
  (define def-proj (project-def tmpl penv))
  (define use-proj (project-use tmpl penv))
  (define marked-def (mark-syntax def-proj def-mark))
  (define marked-use (mark-syntax use-proj use-mark))
  (define marked-stx (combine-projections marked-def marked-use))
  ;; Set use-site span on outermost result if it came from template
  (define use-site-span (and (stx? expr) (stx-span expr)))
  (define final-stx (if use-site-span
                        (maybe-set-use-site-span marked-stx def-mark use-site-span)
                        marked-stx))
  ;; Create disjoin scope
  (define introduced-defn-scp (new-scope def-scp))
  (define disjoined-scp (disjoin def-mark introduced-defn-scp
                                 use-mark use-scp))
  (values final-stx disjoined-scp))

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
;; Pattern variable positions become '_.
;; We preserve surface ids on macro-introduced syntax.
(define (project-def tmpl penv)
  (match tmpl
    [(stx e node-id spn marks)
     (cond
       [(and (identifier? tmpl) (hash-has-key? penv (identifier->key tmpl)))
        '_]  ; will be filled by use-site syntax
       [(list? e)
        (stx (map (lambda (t) (project-def t penv)) e)
             node-id spn marks)]
       [(pair? e)
        (stx (cons (project-def (car e) penv)
                   (project-def (cdr e) penv))
             node-id spn marks)]
       [(identifier? tmpl)
        (stx e node-id spn marks)]
       [else
        (stx e node-id spn marks)])]
    [_ tmpl]))  ; numbers pass through

;; project-use : Syntax PatternEnv -> Projection
;; Creates the use-site projection of a template.
;; Pattern variables are replaced with their matched syntax.
(define (project-use tmpl penv)
  (match tmpl
    [(stx e _ _ _)
     (cond
       [(and (identifier? tmpl) (hash-has-key? penv (identifier->key tmpl)))
        (hash-ref penv (identifier->key tmpl))]
       [(list? e)
        (map (lambda (t) (project-use t penv)) e)]
       [(pair? e)
        (cons (project-use (car e) penv) (project-use (cdr e) penv))]
       [else '_])]
    ['() '()]
    [_ '_]))

;; combine-projections : Projection Projection -> Syntax
;; Merges two projections back into a single syntax object.
;; '_ in one projection is filled by the corresponding part of the other.
(define (combine-projections proj1 proj2)
  (match* (proj1 proj2)
    [('_ proj2) proj2]  ; use-site syntax keeps its ID and span
    [(proj1 '_) proj1]  ; template syntax has id=#f
    [(same same) same]
    ;; stx list + plain list (from project-use)
    [((stx (? list? p1elems) id1 spn1 marks1) (? list? p2elems))
     (stx (map combine-projections p1elems p2elems) id1 spn1 marks1)]
    [((? list? p1elems) (stx (? list? p2elems) id2 spn2 marks2))
     (stx (map combine-projections p1elems p2elems) id2 spn2 marks2)]
    [((? list? p1elems) (? list? p2elems))
     (map combine-projections p1elems p2elems)]
    ;; cons pairs (dotted)
    [((stx (cons p1a p1d) id1 spn1 marks1) (cons p2a p2d))
     (stx (cons (combine-projections p1a p2a)
                (combine-projections p1d p2d))
          id1 spn1 marks1)]
    [((cons p1a p1d) (stx (cons p2a p2d) id2 spn2 marks2))
     (stx (cons (combine-projections p1a p2a)
                (combine-projections p1d p2d))
          id2 spn2 marks2)]
    [((cons p1a p1d) (cons p2a p2d))
     (cons (combine-projections p1a p2a)
           (combine-projections p1d p2d))]))

;; maybe-set-use-site-span : Syntax Mark Span -> Syntax
;; If the outermost result has the def-site mark, it came from the template
;; and should get the use-site span for error reporting.
(define (maybe-set-use-site-span result def-mark use-site-span)
  (match result
    [(stx e id _ (cons (== def-mark) rest-marks))
     ;; Has def-site mark: came from template, use the use-site span
     (stx e id use-site-span (cons def-mark rest-marks))]
    [_ result]))  ; has use-site mark or other: keep existing span

;; ============================================================
;; Syntax-Rules Matching
;; ============================================================

;; select-syntax-rule : Syntax Syntax Scope Scope -> (values PatternEnv Syntax)
;; Selects the first matching clause from a syntax-rules transformer.
;; Returns the pattern environment (bindings of pattern variables) and
;; the template to instantiate.
(define (select-syntax-rule macrot expr def-scp use-scp)
  ;; macrot is (syntax-rules (literal ...) clause ...)
  (define macrot-elems (stx-list macrot))
  (define literals-stx (list-ref macrot-elems 1))
  (define literal-id* (stx-list literals-stx))
  (define clause* (cddr macrot-elems))
  (define is-literal? (make-is-literal? literal-id*))
  (define literal-match? (make-literal-match? def-scp use-scp))
  (try-clauses clause* expr is-literal? literal-match?))

;; try-clauses : [Listof Clause] Syntax (Id -> Bool) (Id Id -> Bool) -> (values PatternEnv Syntax)
;; Tries each clause in order until one matches.
;; A Clause is (list Pattern Template).
(define (try-clauses clauses expr is-literal? literal-match?)
  (match clauses
    [(cons clause rest)
     ;; clause is [pat tmpl]
     (define clause-elems (stx-list clause))
     (define pat (list-ref clause-elems 0))
     (define tmpl (list-ref clause-elems 1))
     (define maybe-penv (match-top-pattern pat expr is-literal? literal-match?))
     (if maybe-penv
         (values maybe-penv tmpl)
         (try-clauses rest expr is-literal? literal-match?))]
    ['() (error 'syntax-rules "no pattern matched")]))

;; match-top-pattern : Pattern Syntax (Id -> Bool) (Id Id -> Bool) -> (or PatternEnv #f)
;; Matches a top-level pattern against syntax.
;; The car of both pattern and syntax is the macro name (ignored per syntax-rules semantics).
;; Returns a PatternEnv on success, #f on failure.
(define (match-top-pattern pat expr is-literal? literal-match?)
  (match* (pat expr)
    [((stx (list _ pd ...) _ _ _) (stx (list _ ed ...) _ _ _))
     (match-pattern-list pd ed is-literal? literal-match?)]
    [((stx (cons _ pd) _ _ _) (stx (cons _ ed) _ _ _))
     (match-pattern pd ed is-literal? literal-match?)]
    [(_ _) #f]))

;; match-pattern-list : [Listof Pattern] [Listof Syntax] ... -> (or PatternEnv #f)
;; Matches a list of patterns against a list of syntax elements.
(define (match-pattern-list pats exprs is-literal? literal-match?)
  (cond
    [(and (null? pats) (null? exprs)) (hash)]
    [(or (null? pats) (null? exprs)) #f]
    [else
     (define resa (match-pattern (car pats) (car exprs) is-literal? literal-match?))
     (and resa
          (let ([resd (match-pattern-list (cdr pats) (cdr exprs) is-literal? literal-match?)])
            (and resd (hash-union resa resd))))]))

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
    ;; pvar: matches any syntax, binds it using IdentifierKey
    [((? identifier? pvar) syn)
     #:when (not (is-literal? pvar))
     (hash (identifier->key pvar) syn)]
    ;; datum literal: must be equal
    [((stx (? number? v1) _ _ _) (stx (? number? v2) _ _ _))
     #:when (= v1 v2)
     (hash)]
    ;; list: match element-wise
    [((stx (? list? pelems) _ _ _) (stx (? list? eelems) _ _ _))
     (match-pattern-list pelems eelems is-literal? literal-match?)]
    ;; cons (dotted pair): match both parts and combine environments
    [((stx (cons pa pd) _ _ _) (stx (cons ea ed) _ _ _))
     (let ([resa (match-pattern pa ea is-literal? literal-match?)])
       (and resa
            (let ([resd (match-pattern pd ed is-literal? literal-match?)])
              (and resd
                   (hash-union resa resd)))))]
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
(define (make-literal-match? def-scp use-scp)
  (lambda (literal-id target-id)
    (define literal-binding (scope-resolve-internal def-scp literal-id))
    (define target-binding (scope-resolve-internal use-scp target-id))
    (or (and (not (unbound? literal-binding)) (not (unbound? target-binding))
             (eq? literal-binding target-binding))
        (and (unbound? literal-binding) (unbound? target-binding)
             (eq? (identifier-symbol literal-id) (identifier-symbol target-id))))))

;; ============================================================
;; Scope Operations
;; ============================================================

;; new-scope : Scope -> Scope
;; Creates a new scope vertex with the given parent and empty bindings.
(define (new-scope parent)
  (scope parent (make-hash)))

;; scope-resolve : Scope Identifier -> (or Binding unbound)
;; Resolves an identifier by traversing parent edges in the scope graph.
;; Records the resolution in the LSP tables.
(define (scope-resolve scp id)
  (define binding (scope-resolve-internal scp id))
  ;; Record the resolution for LSP
  (record-resolution! id (if (unbound? binding) #f binding) scp)
  binding)

;; scope-resolve-internal : Scope Identifier -> (or Binding unbound)
;; Internal resolution without recording (to avoid double-recording).
(define (scope-resolve-internal scp id)
  (define key (identifier->key id))
  (match scp
    [(core-scope core-bindings)
     (hash-ref core-bindings key (unbound))]
    [(scope parent bindings)
     (hash-ref bindings key (lambda () (scope-resolve-internal parent id)))]
    [(disjoin mark1 scope1 mark2 scope2)
     (cond
       [(top-mark=? id mark1) (scope-resolve-internal scope1 (drop-top-mark id))]
       [(top-mark=? id mark2) (scope-resolve-internal scope2 (drop-top-mark id))]
       [else (unbound)])]))

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
     ;; Record binding site as self-reference for LSP
     ;; Use parent scope so autocomplete at binding site shows pre-binding names
     (record-resolution! id bnd parent)]
    [(disjoin mark1 scope1 mark2 scope2)
     (cond
       [(top-mark=? id mark1) (scope-bind! scope1 (drop-top-mark id) bnd)]
       [(top-mark=? id mark2) (scope-bind! scope2 (drop-top-mark id) bnd)]
       [else (error 'scope-bind "malformed environment")])]))

;; ============================================================
;; Identifier Operations
;; ============================================================

;; mark-syntax : Syntax Mark -> Syntax
;; Recursively marks all identifiers in a syntax object, preserving id/span.
;; Also handles projections which may be plain conses.
(define (mark-syntax syn mark)
  (match syn
    [(stx e node-id spn marks)
     (stx (cond
            [(list? e) (map (lambda (s) (mark-syntax s mark)) e)]
            [(pair? e) (cons (mark-syntax (car e) mark)
                            (mark-syntax (cdr e) mark))]
            [else e])
          node-id spn (cons mark marks))]
    [(cons a d)
     ;; Handle plain cons (from projections)
     (cons (mark-syntax a mark) (mark-syntax d mark))]
    [(? list? elems)
     ;; Handle plain list (from projections)
     (map (lambda (s) (mark-syntax s mark)) elems)]
    [_ syn]))  ; '_, numbers, '() pass through

;; top-mark=? : Identifier Mark -> Boolean
;; Returns #t if the identifier's top mark equals the given mark.
(define (top-mark=? id mark)
  (match id
    [(stx _ _ _ (cons (== mark) _)) #t]
    [_ #f]))

;; drop-top-mark : Identifier -> Identifier
;; Removes the top mark from an identifier's mark stack.
;; Precondition: The identifier has at least one mark.
(define (drop-top-mark id)
  (match id
    [(stx e node-id spn (cons _ marks-rest))
     (stx e node-id spn marks-rest)]))

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
  (expander-state (make-hasheq) (make-hasheq) (make-hasheq)))

;; hash-cons! : MutableHash Key Value -> Void
;; Appends a value to the list stored at key (multi-valued hash).
(define (hash-cons! ht key val)
  (hash-set! ht key (cons val (hash-ref ht key '()))))

;; record-stx! : Stx -> Void
;; Records a surface syntax node in the id->stx table.
(define (record-stx! stx)
  (define id (stx-id stx))
  (define state (current-expander-state))
  (when (and id state)
    (hash-set! (expander-state-id->stx state) id stx)))

;; record-resolution! : Stx Binding-or-#f Scope -> Void
;; Records a resolution in the tables.
;; - Adds to resolutions table for goto-definition and autocomplete
;; - Adds to references table for find-references (if binding has surface site)
(define (record-resolution! ref-stx binding scp)
  (define ref-id (stx-id ref-stx))
  (define state (current-expander-state))
  ;; TODO if state is #f, we should error instead of silently failing
  (when (and ref-id state)
    ;; Record the resolution
    (hash-cons! (expander-state-resolutions state) ref-id
                (resolution binding ref-stx scp))
    ;; If binding has a surface site, record in references table
    (define site (and binding (binding-site binding)))
    (define def-id (and site (stx-id site)))
    (when def-id
      (hash-cons! (expander-state-references state) def-id ref-id))))

;; binding-site : Binding -> (or/c Identifier #f)
;; Extract the binding site identifier from a binding.
(define (binding-site bnd)
  (match bnd
    [(var-binding site _) site]
    [(macro-binding site _ _) site]
    [(keyword-binding _) #f]))

;; record-all-stx! : Syntax -> Void
;; Records all surface syntax nodes in the id->stx table.
(define (record-all-stx! root-stx)
  (let loop ([syn root-stx])
    (match syn
      [(stx e id _spn _marks)
       (when id (record-stx! syn))
       (cond
         [(list? e) (for-each loop e)]
         [(pair? e) (loop (car e)) (loop (cdr e))])]
      [_ (void)])))

;; find-stx-errors : XSExpr -> (Listof StxError)
;; Collects all stx-error values from the expanded s-expression tree.
(define (find-stx-errors expanded)
  (let loop ([syn expanded] [acc '()])
    (match syn
      [(? stx-error? err) (cons err acc)]
      [(stx e _ _ _)
       (cond
         [(list? e) (foldl (lambda (s a) (loop s a)) acc e)]
         [(pair? e) (loop (cdr e) (loop (car e) acc))]
         [else acc])]
      [(cons a d) (loop d (loop a acc))]
      [_ acc])))

;; ============================================================
;; Query Functions
;; ============================================================

;; get-binding-sites-of : ExpanderResult Stx -> (Listof Stx)
;; Given a reference site, returns the binding sites it resolves to.
;; Filters out #f results (core keywords have no binding site).
;; Returns empty list if the node has no Surface_Node_ID or no recorded resolutions.
(define (get-binding-sites-of result ref-stx)
  (define ref-id (stx-id ref-stx))
  (define resolutions (expander-state-resolutions (expander-result-state result)))
  (if ref-id
      (remove-duplicates
       (filter-map
        (lambda (res)
          (define bnd (resolution-binding res))
          (and bnd (binding-site bnd)))
        (hash-ref resolutions ref-id '()))
       #:key stx-id)
      '()))

;; get-reference-sites-of : ExpanderResult Stx -> (Listof Stx)
;; Given a binding site OR reference site, returns all reference sites.
;; If given a binding site, returns all references to that binding.
;; If given a reference site, first finds its binding sites, then finds
;; all references to those binding sites.
;; Includes the binding site itself (binding sites are self-references).
(define (get-reference-sites-of result stx-node)
  (define id (stx-id stx-node))
  (define state (expander-result-state result))
  (define references (expander-state-references state))
  (define id->stx (expander-state-id->stx state))
  (if id
      ;; First check if this is a binding site (has entries in references table)
      (let ([direct-refs (hash-ref references id '())])
        (if (not (null? direct-refs))
            ;; It's a binding site - return all references to it (deduplicated)
            (remove-duplicates
             (for/list ([ref-id direct-refs])
               (hash-ref id->stx ref-id))
             #:key stx-id)
            ;; It's a reference site - find binding sites, then their references
            (let* ([bsites (get-binding-sites-of result stx-node)]
                   [binding-ids (filter-map stx-id bsites)])
              (remove-duplicates
               (append-map
                (lambda (def-id)
                  (for/list ([ref-id (hash-ref references def-id '())])
                    (hash-ref id->stx ref-id)))
                binding-ids)
               #:key stx-id))))
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
    [(disjoin mark1 scope1 mark2 scope2)
     (cond
       [(and (pair? marks) (eq? (car marks) mark1))
        (scope->names scope1 (cdr marks))]
       [(and (pair? marks) (eq? (car marks) mark2))
        (scope->names scope2 (cdr marks))]
       [else (seteq)])]))

;; get-names-in-scope : ExpanderResult Stx -> (Set Symbol)
;; Returns the names in scope at the given syntax node.
;; Uses the Resolution's ref-stx marks and scope to traverse the graph.
;; If the node was resolved under multiple scopes (macro duplication),
;; returns the intersection of names from all scopes.
(define (get-names-in-scope result stx-node)
  (define id (stx-id stx-node))
  (define resolutions (expander-state-resolutions (expander-result-state result)))
  (if id
      (let ([res-list (hash-ref resolutions id '())])
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
  (define id->stx (expander-state-id->stx state))
  (for/list ([def-id (in-hash-keys references)])
    (hash-ref id->stx def-id)))

;; ============================================================
;; Position-Based Query Functions
;; ============================================================

;; Reserved ID for cursor
(define cursor-id -1)

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
      [(stx e id spn _marks)
       (cond
         ;; If this node has a span and contains the position, check children first
         [(and spn (span-contains? spn pos))
          (cond
            [(list? e)
             (or (ormap loop e)
                 (and id syn))]
            [(pair? e)
             (or (loop (car e))
                 (loop (cdr e))
                 (and id syn))]
            [else
             (and id syn)])]
         ;; No span or doesn't contain position - try children anyway
         [(list? e) (ormap loop e)]
         [(pair? e) (or (loop (car e)) (loop (cdr e)))]
         [else #f])]
      [_ #f])))

;; span-contains? : Span Loc -> Boolean
;; Returns #t if the span contains the given position.
(define (span-contains? spn pos)
  (and (loc<=? (span-start spn) pos)
       (loc<? pos (span-end spn))))

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
(define (goto-definition result pos)
  (define node (find-node-at-position result pos))
  (if node
      (filter-map stx-span (get-binding-sites-of result node))
      '()))

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
;; It's a normal surface identifier with a gensym'd name and reserved ID.
(define (make-cursor pos)
  (define zero-span (span pos pos))
  ;; Use Racket's built-in gensym to avoid needing gensym-ctr parameter
  (stx (string->symbol (format "cursor~a" (random 1000000))) cursor-id zero-span '()))

;; insert-cursor-at : Stx Loc -> Stx
;; Inserts a cursor identifier at the given position in the syntax tree.
;; Finds the appropriate list position and inserts the cursor there.
(define (insert-cursor-at root pos cursor)
  (let loop ([syn root])
    (match syn
      [(stx e id spn marks)
       (cond
         ;; Empty list node - insert cursor here if position is nearby
         [(and (null? e) spn (span-contains-or-at? spn pos))
          (stx (list cursor) id spn marks)]
         ;; If this is a list node and the position is inside it,
         ;; try to insert the cursor in the right place
         [(and spn (list? e) (not (null? e)) (span-contains? spn pos))
          (define new-elems (map loop e))
          (if (equal? new-elems e)
              ;; No child changed, insert cursor at the right position
              (stx (insert-cursor-in-elems e pos cursor) id spn marks)
              (stx new-elems id spn marks))]
         [(and (list? e) (not (null? e)))
          (define new-elems (map loop e))
          (if (equal? new-elems e) syn (stx new-elems id spn marks))]
         ;; Dotted pair
         [(and spn (pair? e) (span-contains? spn pos))
          (define new-car (loop (car e)))
          (define new-cdr (loop (cdr e)))
          (if (and (eq? new-car (car e)) (eq? new-cdr (cdr e)))
              (stx (cons new-car new-cdr) id spn marks)
              (stx (cons new-car new-cdr) id spn marks))]
         [(pair? e)
          (define new-car (loop (car e)))
          (define new-cdr (loop (cdr e)))
          (if (and (eq? new-car (car e)) (eq? new-cdr (cdr e)))
              syn
              (stx (cons new-car new-cdr) id spn marks))]
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
       (define x2 1)
       (begin))
     (begin
       (define x5 2)
       (#%expression x2))))

  ;; make sure dotted patterns work
  (check-match
   (expand
    '(let-syntax ([m (syntax-rules () [(m . a) (let ([a 2]) a)])])
        (m . a)))
   '(let ([a2 2]) a2))
  
  ;; dotted (a . (b)) = (a b)
  (displayln "skipping dot paren test")
  #;
  (check-match
   (expand
    '(#%expression . (2)))
   '2))

