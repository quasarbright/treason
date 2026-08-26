#lang racket
(require (only-in racket/base [gensym racket-gensym]) racket/set)

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
;;       | (block def-or-expr ... expr) ;; must end in an expression
;;       | (let ([var expr]) expr)
;;       | (let-syntax ([mname macrot]) expr)
;;       | (mname ustx ...)
;; def-or-expr := expr
;;              | (define-syntax mname macrot)
;;              | (define var expr)
;;              | (begin def-or-expr ...)
;;              | (#%expression expr)
;;              | (mname ustx ...)
;;
;; macrot := (syntax-rules (id ...) [(_ pat ...) tmpl] ...)
;;
;; ustx := var | expr
;;
;; pat := (pat . pat)
;;      | ()
;;      | number
;;      | id
;;      | (~var x expr) ;; annotated variable. this is a divergence from syntax-rules
;;      | x:expr        ;; shorthand for (~var x expr)
;;      | (pat ...)     ;; ellipsis: zero or more, only at end of a list pattern
;;
;; tmpl := (tmpl . tmpl)
;;       | ()
;;       | number
;;       | id
;;       | id that is pvar
;;       | (tmpl ...)   ;; ellipsis: repeat tmpl once per matched ellipsis element

(provide (all-defined-out))
(require "stx.rkt")
(require "stx-quote.rkt")
(require "reader.rkt")
(require racket/hash)
(require racket/pretty)

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

;; ------------------------------------------------------------
;; LSP State
;; ------------------------------------------------------------

;; An ExpanderState is a (expander-state Hash Hash Hash Hash MutableSet)
(struct expander-state [resolutions references bindings stx-errors] #:transparent)
;; resolutions : [MutableHashOf Span [Listof Resolution]] - maps ref span to resolutions
;; references : [MutableHashOf Span [Listof Stx]] - maps binding site span to ref stx nodes
;; bindings : [MutableHashOf Span Binding] - maps binding site span to its Binding
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

;; An ExpanderResult is a (expander-result [Listof Stx] XSExpr (Listof StxError) ExpanderState)
(struct expander-result [surface expanded errors state] #:transparent)
;; surface : [Listof Stx] - the original top-level surface syntax forms
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

;; A Progress is a (Listof ProgressStep).
;; It records how deeply into a syntax-rules pattern structure matching reached before failing.
;; Each step corresponds to a structural descent: 'first for the car of a pair, 'rest for the cdr.
;; A longer Progress means the pattern matched more structure before failing, so when multiple
;; clauses all fail, the one with the longest (deepest) Progress is the best candidate for
;; optimistic subexpression expansion.
;;
;; A ProgressStep is one of:
;; - 'first : descended into the car of a pair
;; - 'rest  : descended into the cdr of a pair

;; A MatchResult is (match-result (or PatternEnv #f) Progress (Listof Stx))
;; Represents the outcome of matching a single syntax-rules pattern against a macro call.
;; On success, penv holds the pattern variable bindings and ose-stxs holds any ~var-marked
;; subexpressions (which may still be expanded even on success). On failure, penv is #f,
;; progress records how far matching got, and ose-stxs holds the ~var subexpressions
;; collected so far — used for optimistic subexpression expansion so the expander can
;; continue providing LSP services inside a macro call that doesn't match any clause.
;;
;; penv     : (or PatternEnv #f) — pattern variable bindings on success, #f on failure
;; progress : Progress — how far into the pattern structure matching reached (used on failure
;;            to select the best-matching clause across multiple syntax-rules clauses)
;; ose-stxs : (Listof Stx) — subexpressions tagged with (~var id expr) in the pattern,
;;            collected regardless of success or failure
(struct match-result [penv progress ose-stxs] #:transparent)

;; An OSEError is a (ose-error Symbol String Stx (or Stx #f) (Listof Stx))
;; A stx-error for a macro call that matched no clause, carrying the ~var-tagged
;; subexpressions whose optimistic expansion has not happened yet.
;;
;; The expansion is left to whoever catches the error, because only the catcher
;; knows when it is safe to run: in an expression context that is immediately, but
;; in a definition context pass 1 has not yet discovered every binding, so it must
;; wait until pass 2. The catcher expands them in the scope it passed to
;; expand-macro, which is by construction the scope the match was attempted in.
;;
;; exprs : (Listof Stx) — subexpressions awaiting optimistic expansion
(struct ose-error stx-error [exprs] #:transparent)

;; match-result-success? : MatchResult -> Boolean
(define (match-result-success? r)
  (and (match-result-penv r) #t))

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

;; analyze! : [Listof Stx] -> ExpanderResult
;; Main entry point. Initializes state, records surface syntax, expands, returns result.
(define (analyze! surface-stxs)
  (define state (make-expander-state))
  (parameterize ([current-expander-state state]
                 [gensym-ctr 0])
    (define expanded (expand-toplevel surface-stxs initial-scope))
    (define errors (for/list ([err (expander-state-stx-errors state)]) err))
    (expander-result surface-stxs expanded errors state)))

;; expand-toplevel : [Listof Stx] Scope -> XSExpr
;; Expands a list of top-level forms as an implicit block body.
(define (expand-toplevel stxs scp)
  (define scp^ (new-scope scp))
  (define defs^ (expand-defs-pass1 stxs scp^))
  (define defs^^ (expand-defs-pass2 defs^ scp^))
  `(block ,@defs^^))

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
    [(app stx-e (? boolean? b)) b]
    [(? identifier? id)
     (define binding (scope-resolve scp id))
     (cond
       [(var-binding? binding) (var-binding-name binding)]
       [(keyword-binding? binding) (identifier-symbol id)]
       [(stx-error? binding) binding]
       [else (record-and-return-stx-error
              (stx-error 'expand-expr "unexpected binding type" expr #f))])]
    [(stx-quote (,head-stx . ,_))
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
        (check-block-tail! expr defs defs^^)
        `(block . ,defs^^)]
       ;; let
       [(and (keyword-binding? binding)
             (eq? 'let (keyword-binding-name binding)))
        (with-stx-error-handling
          (match expr
            [(stx-quote (let ([,(and x-stx (? identifier?)) ,e-stx]) ,body))
             (define e^ (expand-expr e-stx scp))
             (define scp^ (new-scope scp))
             (define x-name (gensym (identifier-symbol x-stx)))
             (define x-binding (var-binding x-stx x-name))
             (scope-bind! scp^ x-stx x-binding)
             (define b^ (expand-expr body scp^))
             `(let ([,x-name ,e^]) ,b^)]
            ;; optimistic sub-expression expansion
            [(stx-quote (let ,bg ,body))
             (define bg^
               (match bg
                 [(stx-quote ([,bad ,e]))
                  (define err (stx-error 'let "bad syntax" expr bad))
                  (record-stx-error! err)
                  `([,err ,(expand-expr e scp)])]
                 [_
                  (define err (stx-error 'let "bad syntax" expr bg))
                  (record-stx-error! err)
                  err]))
             (define body^ (expand-expr body scp))
             `(let ,bg^ ,body^)]
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
             (record-all-pvar-resolutions-for-macrot! macrot-stx)
             (expand-expr body scp^)]
            ;; optimistic sub-expression expansion
            [(stx-quote (let-syntax ,bg ,body))
             ;; stx error actually doesn't show up in expanded output
             (record-stx-error! (stx-error 'let-syntax "bad syntax" expr bg))
             (expand-expr body scp)]
            [_ (raise-and-record-stx-error (stx-error 'let-syntax "bad syntax" expr #f))]))]
       ;; macro application
       [(macro-binding? binding)
        ;; NOTE: when no clause matches, expand-macro raises an ose-error carrying
        ;; subexpressions to optimistically expand. This is an expression context, so
        ;; every binding is already known and they can be expanded right away.
        (define result
          (with-stx-error-handling
            (define-values (marked-stx disjoined-scp) (expand-macro head-stx expr scp))
            (expand-expr marked-stx disjoined-scp)))
        (when (ose-error? result)
          (for ([e (ose-error-exprs result)]) (expand-expr e scp)))
        result]
       ;; unbound in head position - pass through the stx-error from scope-resolve
       [(stx-error? binding)
        binding]
       ;; variable in head position - not callable
       [(var-binding? binding)
        (record-and-return-stx-error
         (stx-error (identifier-symbol head-stx) "not a procedure or syntax" expr head-stx))]
       [else (record-and-return-stx-error
              (stx-error #f "unexpected form" expr head-stx))])]
    ;; Non-identifier in head position
    [(stx-quote (,head-stx . ,_))
     (record-and-return-stx-error
      (stx-error #f "not a procedure or syntax" expr head-stx))]))

;; check-block-tail! : Stx [Listof Stx] [Listof XDef] -> Void
;; Records a stx-error unless the block's last form is an expression.
;; A block is an expression, so it must end in one for its value to be defined.
;; This is checked after expansion because a macro in tail position may expand
;; to either a definition or an expression.
;; surface and expanded are index-aligned: both passes map over the same list.
(define (check-block-tail! expr surface expanded)
  (unless (xdefs-end-in-expression? expanded)
    (record-stx-error!
     (stx-error 'block "block must end in an expression" expr
                (and (pair? surface) (last surface))))))

;; xdefs-end-in-expression? : [Listof XDef] -> Boolean
;; Does this block body end in an expression? An empty body does not.
;; A begin is checked through its own body, since it splices into this one.
;; An error node counts as an expression so a broken tail is not reported twice.
(define (xdefs-end-in-expression? defs)
  (and (pair? defs)
       (match (last defs)
         [(? stx-error?) #t]
         [`(#%expression ,_) #t]
         [`(begin ,defs^ ...) (xdefs-end-in-expression? defs^)]
         [_ #f])))

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
             (record-all-pvar-resolutions-for-macrot! macrot-stx)
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
       ;; NOTE: when no clause matches, expand-macro raises an ose-error. Its
       ;; subexpressions are NOT expanded here — pass 1 has not discovered every
       ;; binding yet, so expanding now would resolve them against an incomplete
       ;; scope. The error is returned as this def's Pass1Def and pass 2 expands them.
       [(macro-binding? binding)
        (with-stx-error-handling
          (define-values (marked-stx disjoined-scp) (expand-macro head-stx def scp))
          (with-disjoin (expand-def-pass1 marked-stx disjoined-scp) disjoined-scp))]
       ;; unbound in head position - treat as expression
       [(stx-error? binding) `(#%expression ,def)]
       ;; variable in head position - treat as expression
       [(var-binding? binding) `(#%expression ,def)]
       ;; other keyword (e.g. let, if, block) in head position - treat as expression
       [else `(#%expression ,def)])]
    ;; bare non-list (e.g. a bare identifier or number) - treat as expression
    [_ `(#%expression ,def)]))

;; expand-def-pass2 : Pass1Def Scope -> XDef
;; Second pass of definition expansion: expands all expressions.
;; The scp argument is the default scope; with-disjoin wrappers
;; override it with their stored disjoin scope.
(define (expand-def-pass2 def scp)
  (match def
    ;; an unmatched macro call from pass 1: now that every binding in this
    ;; definition context is known, optimistically expand its subexpressions
    [(? ose-error?)
     (for ([e (ose-error-exprs def)]) (expand-expr e scp))
     def]
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
;; Ellipsis Utilities
;; ============================================================

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
;; Raises stx-error on bare ..., or on a pvar used at wrong ellipsis depth.
(define (expand-template tmpl penv def-mark)
  (match tmpl
    [(and (stx '... _ _) id)
     (raise-and-record-stx-error
      (stx-error 'syntax-rules "unexpected ellipsis in template" id #f))]
    [(? identifier? id)
     (cond
       [(hash-has-key? penv (identifier->key id))
        (define val (hash-ref penv (identifier->key id)))
        (if (list? val)
            (raise-and-record-stx-error
             (stx-error 'syntax-rules "missing ellipsis in template for pattern variable" id #f))
            val)]
       [else (mark-id id def-mark)])]
    [(stx (? list? elems) spn marks)
     (stx (expand-template-list elems penv def-mark) spn marks)]
    [(stx (cons a d) spn marks)
     (stx (cons (expand-template a penv def-mark)
                (expand-template d penv def-mark))
          spn marks)]
    [_ tmpl]))  ; numbers, booleans, etc. pass through

;; expand-template-list : (Listof Stx) PatternEnv Mark -> (Listof Stx)
;; Expands a list of template elements, splicing (t ...) into repeated expansions.
(define (expand-template-list elems penv def-mark)
  (match elems
    ['() '()]
    [(stx-quote (,t ,(stx '... _ _) . ,rest))
     (define sub-envs (split-env penv t))
     (define expanded (for/list ([sub-env sub-envs])
                        (expand-template t sub-env def-mark)))
     (append expanded (expand-template-list rest penv def-mark))]
    [(cons t rest)
     (cons (expand-template t penv def-mark)
           (expand-template-list rest penv def-mark))]))

;; template-free-variables : Stx -> (Listof IdentifierKey)
;; Returns the IdentifierKeys for all non-ellipsis identifiers in a template.
(define (template-free-variables tmpl)
  (let loop ([t tmpl] [acc '()])
    (cond
      [(and (stx? t) (eq? (stx-e t) '...)) acc]
      [(identifier? t)
       (define key (identifier->key t))
       (if (member key acc) acc (cons key acc))]
      [(stx? t) (loop (stx-e t) acc)]
      [(list? t) (foldl (lambda (elem a) (loop elem a)) acc t)]
      [(pair? t) (loop (car t) (loop (cdr t) acc))]
      [else acc])))

;; split-env : PatternEnv Stx -> (Listof PatternEnv)
;; Splits a pattern environment for one level of ellipsis expansion of template t.
;; Returns one sub-environment per iteration, threading depth-≥1 vars through
;; indexing and passing depth-0 vars unchanged to every iteration (mixed depth).
;; Raises stx-error if all variables are depth-0 (too many ellipses)
;; or if depth-≥1 variables have different match counts.
(define (split-env penv t)
  ;; Only consider vars actually present in penv — non-pvar identifiers in the
  ;; template (e.g. macro-introduced keywords) are not pattern variables and
  ;; must not appear in sub-envs (which would cause expand-template to return
  ;; the #f default instead of marking them with def-mark).
  (define vars (filter (lambda (v) (hash-has-key? penv v))
                       (template-free-variables t)))
  (cond
    [(null? vars) '()]
    [else
     (define rose-vals (for/list ([var vars]) (hash-ref penv var)))
     (define list-vals (filter list? rose-vals))
     (cond
       [(null? list-vals)
        (raise-and-record-stx-error
         (stx-error 'syntax-rules "too many ellipses in template" t #f))
        '()]
       [else
        (define lengths (map length list-vals))
        (unless (apply = lengths)
          (raise-and-record-stx-error
           (stx-error 'syntax-rules "ellipsis variable mismatch in template" t #f)))
        (define len (first lengths))
        (for/list ([i (in-range len)])
          (for/hash ([var vars] [rose rose-vals])
            (values var (if (list? rose) (list-ref rose i) rose))))])]))

;; ============================================================
;; Pattern Variable Annotations
;; ============================================================

;; An annotated pattern variable declares the syntax class of the subexpression
;; it matches. It has two equivalent surface forms:
;;   (~var body expr)
;;   body:expr
;; expr is the only syntax class.
;;
;; NOTE: annotated-pvar is a match expander, so unlike an ordinary helper it must
;; be defined above the pattern code below that uses it.

;; annotated-pvar : match expander
;; Matches either surface form of an annotated pattern variable, binding the
;; pattern variable identifier and the syntax class identifier.
;; ex: (match p [(annotated-pvar pvar (app identifier-symbol 'expr)) ...])
(define-match-expander annotated-pvar
  (syntax-rules ()
    [(_ pvar-pat class-pat)
     (app parse-annotated-pvar (list pvar-pat class-pat))]))

;; parse-annotated-pvar : Stx -> (or/c (List Identifier Identifier) #f)
;; Recognizes an annotated pattern variable in either surface form, returning its
;; pattern variable identifier and its syntax class identifier, or #f if p is not one.
(define (parse-annotated-pvar p)
  (match p
    [(stx-quote (~var ,(? identifier? pvar) ,(? identifier? cls)))
     (list pvar cls)]
    [(? identifier? id) (split-annotated-id id)]
    [_ #f]))

;; split-annotated-id : Identifier -> (or/c (List Identifier Identifier) #f)
;; Splits id:class at its first colon into an identifier for the pattern variable
;; name and one for the syntax class, each spanning only its own part of the token.
;; Returns #f if the identifier has no colon, or nothing on one side of it
;; (e.g. x: or :expr), which makes it an ordinary pattern variable.
;; ex: (split-annotated-id body:expr) = (list body expr)
(define (split-annotated-id id)
  (define str (symbol->string (identifier-symbol id)))
  (define colon-index
    (for/first ([c (in-string str)] [i (in-naturals)] #:when (char=? c #\:)) i))
  (and colon-index
       (> colon-index 0)
       (< colon-index (sub1 (string-length str)))
       (list (substring-identifier id 0 colon-index)
             (substring-identifier id (add1 colon-index) (string-length str)))))

(module+ test
  (define annotated (string->stx "test" "body:expr"))
  (define parts (split-annotated-id annotated))
  (check-equal? (map identifier-symbol parts) '(body expr))
  ;; each part spans only its own text, not the whole token
  (check-equal? (stx-span (first parts)) (span (loc "test" 0 0) (loc "test" 0 4)))
  (check-equal? (stx-span (second parts)) (span (loc "test" 0 5) (loc "test" 0 9)))
  ;; the marks of the token carry over to both parts
  (check-equal? (map stx-marks parts) (list (stx-marks annotated) (stx-marks annotated)))
  ;; identifiers that are not annotations
  (check-false (split-annotated-id (string->stx "test" "body")))
  (check-false (split-annotated-id (string->stx "test" "body:")))
  (check-false (split-annotated-id (string->stx "test" ":expr")))
  (check-false (split-annotated-id (string->stx "test" ":")))
  ;; only the first colon splits
  (check-equal? (map identifier-symbol (split-annotated-id (string->stx "test" "a:b:c")))
                '(a b:c)))

;; substring-identifier : Identifier Natural Natural -> Identifier
;; Makes an identifier from the [start, end) character range of id's name, spanning
;; only that range of id's source and keeping its marks. An identifier is a single
;; token, so it never spans a line break and the range stays within one line.
(define (substring-identifier id start end)
  (define name (symbol->string (identifier-symbol id)))
  (define spn (stx-span id))
  (struct-copy stx id
               [e (string->symbol (substring name start end))]
               [span (and spn (span (loc-shift (span-start spn) start)
                                    (loc-shift (span-start spn) end)))]))

;; loc-shift : Loc Natural -> Loc
;; Moves a location forward by n characters within its line.
(define (loc-shift lc n)
  (struct-copy loc lc [column (+ (loc-column lc) n)]))

;; ============================================================
;; Pattern Variable LSP Resolution
;; ============================================================

;; record-all-pvar-resolutions-for-macrot! : Syntax -> Void
;; Eagerly records LSP pvar resolutions for every clause in a syntax-rules transformer.
;; Called at let-syntax/define-syntax expansion time so that goto-definition,
;; find-references, and autocomplete work on pattern variables even if the macro
;; is never applied.
(define (record-all-pvar-resolutions-for-macrot! macrot)
  (match macrot
    [(stx-quote (,_syntax-rules (,literal-ids ...) ,clauses ...))
     (define is-literal? (make-is-datum-literal? literal-ids))
     (for ([clause clauses])
       (match clause
         [(stx-quote [,pat ,tmpl])
          (define pvar-scp (build-pvar-scope pat is-literal?))
          (record-pvar-resolutions! tmpl pvar-scp)]))]))

;; build-pvar-scope : Pattern (Id -> Bool) -> Scope
;; Builds a scope containing a pattern-variable-binding for each pvar in the pattern.
;; Also records each pvar as a binding site so goto-definition works even if the pvar
;; is never referenced in the template.
;; The scope has no parent: a template is only scanned for pattern variables, since
;; at definition time we can't tell a macro-introduced reference from a binding
;; position. Use-site expansion analyzes macro-introduced identifiers instead.
;; The pattern head is skipped, matching match-top-pattern, which ignores it.
;; The wildcard _ is excluded since it is never referenced in templates.
(define (build-pvar-scope pat is-literal?)
  (define scp (scope (core-scope (hash)) (make-hash)))
  ;; the head names the macro, so only the arguments after it can be pvars
  (define pat-args
    (match pat
      [(stx-quote (,_head . ,rest)) rest]
      [_ '()]))
  ;; bind-pvar! : Identifier -> Void
  ;; Binds id as a pattern variable, unless it is a datum literal, the wildcard _,
  ;; or the ellipsis ... — none of which name anything a template can reference.
  (define (bind-pvar! id)
    (unless (or (is-literal? id)
                (eq? (identifier-symbol id) '_)
                (eq? (identifier-symbol id) '...))
      (scope-bind! scp id (pattern-variable-binding id))))
  (let loop ([p pat-args])
    (match p
      ;; annotated pattern variable: must precede the bare identifier case, so that
      ;; body:expr binds body rather than a pattern variable named body:expr
      [(annotated-pvar id (app identifier-symbol 'expr))
       #:when (not (datum-literal? is-literal? p))
       (bind-pvar! id)]
      ;; unknown syntax class: report it, but still bind the pattern variable so
      ;; template references to it resolve
      [(annotated-pvar id cls)
       #:when (not (datum-literal? is-literal? p))
       (record-stx-error! (stx-error '~var "unknown syntax class" p cls))
       (bind-pvar! id)]
      [(stx-quote (~var . ,_))
       (raise-and-record-stx-error (stx-error '~var "bad syntax" pat p))]
      [(? identifier? id)
       (bind-pvar! id)]
      [(stx-quote (,a . ,d))
       (loop a)
       (loop d)]
      [_ (void)]))
  scp)

;; record-pvar-resolutions! : Syntax Scope -> Void
;; Walks a template and records LSP resolutions for pattern variable identifiers
;; and cursor identifiers. Pattern variable resolutions enable goto-definition and
;; find-references. Cursor resolutions enable cursor-driven autocomplete to include
;; pvars in scope. Non-pvar, non-cursor template identifiers are skipped to avoid
;; spuriously highlighting them (e.g. let, if) in unused macros.
(define (record-pvar-resolutions! tmpl pvar-scp)
  (match tmpl
    [(stx '... _ _) (void)]
    [(? identifier? id)
     (define bnd (scope-resolve-internal pvar-scp id))
     (when (or (pattern-variable-binding? bnd) (cursor-identifier? id))
       (record-resolution! id (if (stx-error? bnd) #f bnd) pvar-scp))]
    [(stx-quote (,a . ,d))
     (record-pvar-resolutions! a pvar-scp)
     (record-pvar-resolutions! d pvar-scp)]
    [_ (void)]))

;; cursor-identifier? : Stx -> Boolean
;; Returns #t if the stx is a cursor — an identifier with a zero-width span
;; and an uninterned gensym'd symbol starting with "cursor".
(define (cursor-identifier? stx-node)
  (and (identifier? stx-node)
       (let ([sym (identifier-symbol stx-node)])
         (and (not (symbol-interned? sym))
              (string-prefix? (symbol->string sym) "cursor")))
       (let ([spn (stx-span stx-node)])
         (and spn (equal? (span-start spn) (span-end spn))))))

;; ============================================================
;; Syntax-Rules Matching
;; ============================================================

;; select-syntax-rule : Symbol Syntax Syntax -> (values PatternEnv Syntax)
;; Selects the first matching clause from a syntax-rules transformer.
;; On failure, raises an ose-error carrying the ~var subexpressions to expand.
(define (select-syntax-rule who macrot expr)
  ;; macrot is (syntax-rules (literal ...) clause ...)
  (match macrot
    [(stx-quote (,_syntax-rules (,literal-ids ...) ,clauses ...))
     (define is-datum-literal? (make-is-datum-literal? literal-ids))
     (try-patterns who clauses expr is-datum-literal?)]))

;; try-patterns : Symbol [Listof Clause] Syntax (Id -> Bool) -> (values PatternEnv Syntax)
;; Tries each clause in order until one matches.
;; On success, returns (values penv tmpl).
;; On failure, raises an ose-error carrying the ~var subexpressions of the
;; best-progress clause(s), leaving their optimistic expansion to the catcher.
;; A Clause is (List pattern template).
(define (try-patterns who clauses expr is-datum-literal?)
  (define results+tmpls
    (for/list ([clause clauses])
      (match clause
        [(stx-quote [,pat ,tmpl])
         (cons (match-top-pattern pat expr is-datum-literal?) tmpl)])))
  (define success
    (for/first ([r+t results+tmpls] #:when (match-result-success? (car r+t)))
      r+t))
  (cond
    [success (values (match-result-penv (car success)) (cdr success))]
    [else
     (define sorted
       (sort results+tmpls
             (lambda (a b)
               (progress<? (match-result-progress (car a))
                           (match-result-progress (car b))))))
     (define oses
       (cond
         [(pair? sorted)
          (define best-progress (match-result-progress (car (last sorted))))
          (define r+t-with-best-progress
            (for/list ([r+t sorted]
                       #:when (equal? (match-result-progress (car r+t)) best-progress))
              r+t))
          (apply set-intersect
                 (for/list ([r+t r+t-with-best-progress])
                   (match-result-ose-stxs (car r+t))))]
         [else '()]))
     (raise-and-record-stx-error (ose-error who "no pattern matched" expr #f oses))]))

;; progress<? : Progress Progress -> Boolean
;; True if p1 represents less progress than p2.
;; Longer progress = deeper matching. Equal-length: lexicographic ('first < 'rest).
(define (progress<? p1 p2)
  (cond
    [(< (length p1) (length p2)) #t]
    [(> (length p1) (length p2)) #f]
    [else
     (match* (p1 p2)
       [('() '()) #f]
       [((cons 'first _) (cons 'rest _)) #t]
       [((cons 'rest _) (cons 'first _)) #f]
       [((cons _ r1) (cons _ r2)) (progress<? r1 r2)])]))

(module+ test
  (require rackunit)
  (check-true  (progress<? '() '(first)))
  (check-true  (progress<? '(first) '(first first)))
  (check-false (progress<? '(first first) '(first)))
  (check-false (progress<? '(first) '(first)))
  (check-true  (progress<? '(first) '(rest)))
  (check-false (progress<? '(rest) '(first)))
  (check-true  (progress<? '(first rest) '(rest first)))
  (check-false (progress<? '(rest first) '(first rest))))

;; match-top-pattern : Pattern Stx (Id -> Bool) -> MatchResult
;; Matches a top-level pattern against syntax.
;; Skips the macro name (first element) per syntax-rules semantics.
(define (match-top-pattern pat expr is-datum-literal?)
  (match* (pat expr)
    [((stx-quote (,_ . ,pd)) (stx-quote (,_ . ,ed)))
     (match-pattern pd ed is-datum-literal? (list 'rest))]
    [(_ _) (match-result #f '() '())]))

;; match-pattern : Pattern Stx (Id -> Bool) [ProgressRev] -> MatchResult
;; Matches a pattern against syntax, accumulating progress in reverse.
;; Continues into both sides of a pair even after one fails, to collect all ~var stxs.
(define (match-pattern pat expr is-datum-literal? [progress-rev '()])
  (match* (pat expr)
    ;; datum literal identifier: succeeds only on equal symbol
    [((? identifier? lit) (? identifier? target-id))
     #:when (is-datum-literal? lit)
     (if (equal? (stx->datum lit) (stx->datum target-id))
         (match-result (hash) (reverse progress-rev) '())
         (match-result #f (reverse progress-rev) '()))]
    ;; annotated pattern variable — (~var e expr) or e:expr — always succeeds and
    ;; records stx for OSE; must appear before the general pair and identifier cases
    [((annotated-pvar pvar (app identifier-symbol 'expr)) syn)
     #:when (not (datum-literal? is-datum-literal? pat))
     (match-result (hash (identifier->key pvar) syn) (reverse progress-rev) (list syn))]
    ;; unknown syntax class: still binds the pattern variable, but no OSE.
    ;; The error is reported once at definition time, by build-pvar-scope.
    [((annotated-pvar pvar _) syn)
     #:when (not (datum-literal? is-datum-literal? pat))
     (match-result (hash (identifier->key pvar) syn) (reverse progress-rev) '())]
    ;; bare ...: not a valid pattern variable
    [((and (stx '... _ _) dots) _)
     (record-stx-error! (stx-error 'syntax-rules "unexpected ellipsis in pattern" dots #f))
     (match-result #f (reverse progress-rev) '())]
    ;; pattern variable identifier: always succeeds, no OSE
    [((? identifier? pvar) syn)
     #:when (not (is-datum-literal? pvar))
     (match-result (hash (identifier->key pvar) syn) (reverse progress-rev) '())]
    ;; ellipsis: (p ...) at the end of a list — match p against each element of the input list.
    ;; Larger patterns like (a b ...) reach this case through the pair case for (a . (b ...)).
    ;; Fails on the FIRST element mismatch (that progress is reported); subsequent
    ;; iterations run only to collect OSE subexpressions, mirroring the cons case.
    ;; Each iteration advances progress by one REST before the FIRST for that element.
    [((stx-quote (,p ,(stx '... _ _))) expr)
     (define input-elems
       (cond [(list? expr) expr]
             [(stx? expr) (let ([e (stx-e expr)]) (and (list? e) e))]
             [else #f]))
     (cond
       [(not input-elems)
        (match-result #f (reverse progress-rev) '())]
       [else
        (let loop ([es input-elems]
                   [inner-progress-rev (cons 'first progress-rev)]
                   [first-failure #f]
                   [all-oses '()]
                   [envs-rev '()])
          (cond
            [(null? es)
             (if first-failure
                 (match-result #f (match-result-progress first-failure) all-oses)
                 (match-result (combine-envs (reverse envs-rev) p is-datum-literal?)
                               (reverse progress-rev)
                               all-oses))]
            [else
             (define r (match-pattern p (car es) is-datum-literal? inner-progress-rev))
             (define next-progress-rev
               (cons 'first (cons 'rest (cdr inner-progress-rev))))
             (loop (cdr es)
                   next-progress-rev
                   (or first-failure (and (not (match-result-success? r)) r))
                   (append all-oses (match-result-ose-stxs r))
                   (if (match-result-success? r)
                       (cons (match-result-penv r) envs-rev)
                       envs-rev))]))])]
    ;; pair: recurse into both sides; continue into cdr even if car fails
    [((stx-quote (,pa . ,pd)) (stx-quote (,ea . ,ed)))
     (define ra (match-pattern pa ea is-datum-literal? (cons 'first progress-rev)))
     (define rd (match-pattern pd ed is-datum-literal? (cons 'rest progress-rev)))
     (define ose (append (match-result-ose-stxs ra) (match-result-ose-stxs rd)))
     (cond
       [(not (match-result-success? ra))
        (match-result #f (match-result-progress ra) ose)]
       [(not (match-result-success? rd))
        (match-result #f (match-result-progress rd) ose)]
       [else
        (let/cc fail
          (match-result
           (hash-union (match-result-penv ra) (match-result-penv rd)
                       #:combine (lambda (va vd)
                                   (if (syntax-same-for-binding? va vd)
                                       va
                                       (fail (match-result #f (reverse progress-rev) ose)))))
           (reverse progress-rev)
           ose))])]
    ;; datum equality: handles literal values and structural mismatches
    [(_ _)
     (if (equal? (stx->datum pat) (stx->datum expr))
         (match-result (hash) (reverse progress-rev) '())
         (match-result #f (reverse progress-rev) '()))]))

;; make-is-datum-literal? : [Listof Identifier] -> (Identifier -> Boolean)
;; Creates a predicate that checks if an identifier is a literal
;; (using bound-identifier=? comparison).
(define (make-is-datum-literal? literal-ids)
  (lambda (id)
    (memf (lambda (x) (eq? (identifier-symbol x) (identifier-symbol id))) literal-ids)))

;; datum-literal? : (Id -> Bool) Stx -> Boolean
;; Returns #t if p is an identifier the transformer declared as a datum literal.
;; Guards the annotated pattern variable cases: a literal named a:b is a literal,
;; not an annotation.
(define (datum-literal? is-literal? p)
  (and (identifier? p) (is-literal? p) #t))

;; combine-envs : (Listof PatternEnv) Stx (Stx -> Boolean) -> PatternEnv
;; Zips per-iteration pattern environments into a single environment with Rose values.
;; Each pattern variable in pat maps to a list of its matched values across iterations.
;; For the empty-list case (no iterations), all pvars map to the empty list.
(define (combine-envs envs pat is-datum-literal?)
  (define vars (pattern-free-variables pat is-datum-literal?))
  (for/hash ([var vars])
    (values var (for/list ([env envs]) (hash-ref env var)))))

;; pattern-free-variables : Stx (Stx -> Boolean) -> (Listof IdentifierKey)
;; Returns the IdentifierKeys for all pattern variables in a pattern.
;; Excludes datum literals, ellipsis (...), and wildcards (_).
(define (pattern-free-variables pat is-datum-literal?)
  (let loop ([p pat] [acc '()])
    (match p
      [(stx '... _ _) acc]
      [(? (lambda (x) (and (identifier? x) (is-datum-literal? x)))) acc]
      [(stx-quote _) acc]
      ;; annotated pattern variable: must precede the bare identifier case
      [(annotated-pvar pvar _)
       (define key (identifier->key pvar))
       (if (member key acc) acc (cons key acc))]
      [(? identifier? id)
       (define key (identifier->key id))
       (if (member key acc) acc (cons key acc))]
      [(stx-quote (,a . ,d))
       (loop a (loop d acc))]
      [(? list? elems)
       (foldl (lambda (elem a) (loop elem a)) acc elems)]
      [(? pair?)
       (loop (car p) (loop (cdr p) acc))]
      [_ acc])))

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
     (raise-and-record-stx-error (stx-error #f "cannot bind in core scope" id #f))]
    [(scope parent bindings)
     (when (hash-has-key? bindings key)
       (raise-and-record-stx-error (stx-error #f (format "name already bound: ~a" (identifier-symbol id)) id #f)))
     (hash-set! bindings key bnd)
     (record-binding-site! id bnd)]
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

;; with-stx-error-handling : catches raised stx-error? and returns it
;; Use this instead of (with-handlers ([exn:fail? ...])) so unexpected errors are not silently swallowed.
(define-syntax-rule (with-stx-error-handling body ...)
  (with-handlers ([stx-error? (lambda (err) err)])
    body ...))

;; stx-error? -> void?
(define (raise-and-record-stx-error err)
  (record-stx-error! err)
  (raise err))

;; record-and-return-stx-error : stx-error? -> stx-error?
;; Records an error and returns it for embedding in the expanded output.
;; Use this for errors that become part of the expanded tree rather than
;; unwinding, so that they still reach the diagnostics table.
(define (record-and-return-stx-error err)
  (record-stx-error! err)
  err)

;; stx-error? -> void?
(define (record-stx-error! err)
  (when (current-expander-state)
    (define errs (expander-state-stx-errors (current-expander-state)))
    (set-add! errs err)))

;; make-expander-state : -> ExpanderState
;; Creates a fresh expander state with empty tables.
(define (make-expander-state)
  (expander-state (make-hash) (make-hash) (make-hash) (mutable-set)))

;; hash-cons! : MutableHash Key Value -> Void
;; Appends a value to the list stored at key (multi-valued hash).
(define (hash-cons! ht key val)
  (hash-set! ht key (cons val (hash-ref ht key '()))))

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
    (define site (and binding (not (stx-error? binding)) (binding-site binding)))
    (define def-spn (and site (stx-span site)))
    (when def-spn
      (hash-cons! (expander-state-references state) def-spn ref-stx))))

;; binding-site : Binding -> (or/c Identifier #f)
;; Extract the binding site identifier from a binding.
(define (binding-site bnd)
  (match bnd
    [(var-binding site _) site]
    [(macro-binding site _ _) site]
    [(pattern-variable-binding site) site]
    [(keyword-binding _) #f]))

;; record-binding-site! : Identifier Binding -> Void
;; Records the binding site in the bindings table (for semantic tokens and LSP).
;; Also ensures the binding site's span is registered in the references table.
(define (record-binding-site! id bnd)
  (define spn (stx-span id))
  (define state (current-expander-state))
  (when (and spn state)
    (hash-set! (expander-state-bindings state) spn bnd)
    (define references (expander-state-references state))
    (unless (hash-has-key? references spn)
      (hash-set! references spn '()))))

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
  (if spn
      ;; First check if this is a binding site (has entry in bindings table)
      (let ([is-binding-site? (hash-has-key? (expander-state-bindings state) spn)])
        (if is-binding-site?
            ;; It's a binding site - return all references to it (deduplicated)
            (remove-duplicates
             (hash-ref references spn '())
             #:key stx-span)
            ;; It's a reference site - find binding sites, then their references
            (let* ([bsites (get-binding-sites-of result stx-node)]
                   [binding-spns (filter-map stx-span bsites)])
              (remove-duplicates
               (append-map
                (lambda (def-spn)
                  (hash-ref references def-spn '()))
                binding-spns)
               #:key stx-span))))
      '()))

;; scope->names : Scope (Listof Mark) #:include-binding? [Binding -> Boolean] -> (Set Symbol)
;; Traverse scope graph to collect names accessible with the given marks.
;; Only returns names whose binding keys have marks matching the current marks.
;; At disjoins, drops the top mark when traversing the matching edge.
(define (scope->names scp marks #:include-binding? [include-binding? (lambda (_) #t)])
  (match scp
    [(core-scope bindings)
     (for/seteq ([(key bnd) (in-hash bindings)]
                 #:when (equal? (identifier-key-marks key) marks)
                 #:when (include-binding? bnd))
       (identifier-key-symbol key))]
    [(scope parent bindings)
     (set-union
      (for/seteq ([(key bnd) (in-hash bindings)]
                  #:when (equal? (identifier-key-marks key) marks)
                  #:when (include-binding? bnd))
        (identifier-key-symbol key))
      (scope->names parent marks #:include-binding? include-binding?))]
    [(disjoin def-mark def-scp use-scp)
     (cond
       [(and (pair? marks) (eq? (car marks) def-mark))
        (scope->names def-scp (cdr marks) #:include-binding? include-binding?)]
       [else
        (scope->names use-scp marks #:include-binding? include-binding?)])]))

;; get-names-available-at : ExpanderResult Stx -> (Set Symbol)
;; Returns the names in scope at the given syntax node.
;; Uses the Resolution's ref-stx marks and scope to traverse the graph.
;; If the node was resolved under multiple scopes (macro duplication),
;; returns the union of names from all scopes.
(define (get-names-available-at result stx-node)
  (let/ec return
    (define spn (stx-span stx-node))
    (unless spn (return (seteq)))
    (define resolutions (expander-state-resolutions (expander-result-state result)))
    (define res-list (hash-ref resolutions spn '()))
    (when (null? res-list) (return (seteq)))
    (apply set-union (seteq)
           (for/list ([res res-list])
             (scope->names (resolution-scp res)
                           (identifier-marks (resolution-ref-stx res)))))))

;; visit-surface-stx! : ExpanderResult (Stx -> Void) -> Void
;; Calls f on every node in the surface syntax trees of result.
(define (visit-surface-stx! result f)
  (let loop ([stx-node (expander-result-surface result)])
    (cond
      [(list? stx-node) (for ([s stx-node]) (loop s))]
      [(stx? stx-node)
       (f stx-node)
       (define e (stx-e stx-node))
       (cond
         [(list? e) (for ([s e]) (loop s))]
         [(pair? e) (loop (car e)) (loop (cdr e))])])))

;; get-all-surface-binding-sites : ExpanderResult -> (Listof Stx)
;; Returns all surface binding sites in the program.
;; A binding site is a surface node that has an entry in the bindings table.
(define (get-all-surface-binding-sites result)
  (define state (expander-result-state result))
  (for/list ([(_spn bnd) (in-hash (expander-state-bindings state))]
             #:when (binding-site bnd))
    (binding-site bnd)))
;; ============================================================

;; find-node-at-position : ExpanderResult Loc -> (or/c Stx #f)
;; Given an LSP position (line+column), find the innermost surface syntax node
;; at that position. Searches all top-level surface syntax trees.
(define (find-node-at-position result pos)
  (define surface (expander-result-surface result))
  (ormap (lambda (s) (find-node-at-position-in s pos)) surface))

;; find-node-at-position-in : Stx Loc -> (or/c Stx #f)
;; Finds the innermost surface syntax node containing the given position.
;; For lists/pairs, position at the opening paren is NOT considered inside.
;; For identifiers/atoms, position at the start IS considered inside.
(define (find-node-at-position-in root pos)
  (let loop ([syn root])
    (match syn
      [(stx e spn _marks)
       (cond
         ;; Lists: pos must be strictly after start and < end
         [(and spn (list? e)
               (loc<? (span-start spn) pos)
               (loc<? pos (span-end spn)))
          (or (ormap loop e) syn)]
         ;; Pairs: pos must be strictly after start and < end
         [(and spn (pair? e)
               (loc<? (span-start spn) pos)
               (loc<? pos (span-end spn)))
          (or (loop (car e)) (loop (cdr e)) syn)]
         ;; Atoms (not list/pair): pos can be at start (>= start and <= end)
         [(and spn (not (or (list? e) (pair? e)))
               (loc<=? (span-start spn) pos)
               (loc<=? pos (span-end spn)))
          syn]
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
  (define node (refine-annotated-node result (find-node-at-position result pos) pos))
  (cond
    [(not node) '()]
    [else
     (define resolution-sites (filter-map stx-span (get-binding-sites-of result node)))
     (define spn (stx-span node))
     (define is-binder? (and spn (hash-has-key? (expander-state-bindings (expander-result-state result)) spn)))
     (define self-span (and is-binder? spn))
     (if self-span
         (remove-duplicates (cons self-span resolution-sites) equal?)
         resolution-sites)]))

;; find-references : ExpanderResult Loc -> (Listof Span)
;; Returns spans of all reference sites for the node at position.
(define (find-references result pos)
  (define node (refine-annotated-node result (find-node-at-position result pos) pos))
  (if node
      (filter-map stx-span (get-reference-sites-of result node))
      '()))

;; refine-annotated-node : ExpanderResult (or/c Stx #f) Loc -> (or/c Stx #f)
;; Narrows a colon-annotated pattern variable token to the part of it containing
;; pos — the pattern variable's name or its syntax class — since the expander
;; records those parts, not the whole token. An ordinary identifier whose name
;; happens to contain a colon has no such record and is returned unchanged.
(define (refine-annotated-node result node pos)
  (define parts (and (identifier? node) (split-annotated-id node)))
  (or (and parts
           (for/first ([part parts]
                       #:when (and (stx-span part)
                                   (span-contains? (stx-span part) pos)
                                   (recorded-span? result (stx-span part))))
             part))
      node))

(module+ test
  ;; q:expr starts at column 37; q is at 37, the colon at 38, expr at 39-42.
  (define pattern-source
    "(let-syntax ([m (syntax-rules () [(_ q:expr) (let ([a q]) a)])]) (m 5))")
  (define pattern-result (analyze! (string->stxs "test" pattern-source)))
  (define annotated-token (find-node-at-position pattern-result (loc "test" 0 37)))
  (check-equal? (identifier-symbol annotated-token) 'q:expr)
  ;; on the pattern variable's name: narrowed to the name, which is the binding site
  (let ([refined (refine-annotated-node pattern-result annotated-token (loc "test" 0 37))])
    (check-equal? (identifier-symbol refined) 'q)
    (check-equal? (stx-span refined) (span (loc "test" 0 37) (loc "test" 0 38))))
  ;; on the syntax class: nothing is recorded there, so the token is left alone
  (check-equal? (refine-annotated-node pattern-result annotated-token (loc "test" 0 40))
                annotated-token)
  ;; an ordinary variable whose name contains a colon is left alone: it is
  ;; recorded under its whole span, not under either part
  (define variable-source "(let ([a:b 1]) a:b)")
  (define variable-result (analyze! (string->stxs "test" variable-source)))
  (define variable-token (find-node-at-position variable-result (loc "test" 0 15)))
  (check-equal? (identifier-symbol variable-token) 'a:b)
  (check-equal? (refine-annotated-node variable-result variable-token (loc "test" 0 15))
                variable-token))

;; recorded-span? : ExpanderResult Span -> Boolean
;; Returns #t if the expander recorded a binding or a resolution at the span.
(define (recorded-span? result spn)
  (define state (expander-result-state result))
  (or (hash-has-key? (expander-state-bindings state) spn)
      (hash-has-key? (expander-state-resolutions state) spn)))

;; autocomplete : ExpanderResult Loc -> (Set Symbol)
;; Returns names in scope at position.
;; If position is at an identifier, replaces it with a cursor and re-expands.
;; Otherwise, inserts a cursor and re-expands to find names in scope.
;; Always uses cursor-driven expansion so special autocomplete behavior is
;; confined to cursor detection and doesn't affect other LSP consumers.
;; autocomplete : ExpanderResult Loc -> (Setof Symbol)
(define (autocomplete result pos)
  (define surface (expander-result-surface result))
  (define cursor (make-cursor pos))
  (define with-cursor (insert-or-replace-cursor surface pos cursor))
  (define cursor-result (analyze! with-cursor))
  ;; Remove the cursor's own symbol from results
  (set-remove (get-names-available-at cursor-result cursor)
              (identifier-symbol cursor)))

;; insert-or-replace-cursor : (Listof Stx) Loc Stx -> (Listof Stx)
;; Inserts or replaces cursor in the syntax tree based on the position.
;; Returns a new list of top-level forms with the cursor inserted.
(define (insert-or-replace-cursor surface pos cursor)
  (define node (find-node-at-position-in-list surface pos))
  (cond
    ;; Case 1: cursor is on an identifier - replace it
    [(and node (identifier? node))
     (map (lambda (s) (replace-node-with-cursor s node cursor)) surface)]
    ;; Case 2: cursor is inside a non-identifier node - insert into it
    [node
     (map (lambda (s) (insert-cursor-at s pos cursor)) surface)]
    ;; Case 3: cursor is not in any node - insert as new top-level form
    [else
     (insert-cursor-at-toplevel surface pos cursor)]))

;; find-node-at-position-in-list : (Listof Stx) Loc -> (or/c Stx #f)
;; Finds the innermost surface syntax node at the given position in a list of forms.
(define (find-node-at-position-in-list forms pos)
  (ormap (lambda (s) (find-node-at-position-in s pos)) forms))

;; make-cursor : Loc -> Stx
;; Creates a cursor identifier at the given position.
;; Uses gensym to produce an uninterned symbol, enabling cursor-identifier? detection.
(define (make-cursor pos)
  (define zero-span (span pos pos))
  (stx (racket-gensym 'cursor) zero-span '()))

;; insert-cursor-at-toplevel : (Listof Stx) Loc Stx -> (Listof Stx)
;; Inserts cursor as a new top-level form at the appropriate position.
;; Compares span endpoints to determine where to insert.
(define (insert-cursor-at-toplevel forms pos cursor)
  (insert-cursor-in-list forms pos cursor))

;; insert-cursor-in-list : (Listof Stx) Loc Stx -> (Listof Stx)
;; Inserts cursor into a list of stx elements at the appropriate position.
;; Compares position with span starts to find insertion point.
;; If pos <= span-start of an element, insert cursor before that element.
(define (insert-cursor-in-list elems pos cursor)
  (cond
    [(null? elems) (list cursor)]
    [else
     (define head (car elems))
     (define rest (cdr elems))
     (define spn (stx-span head))
     (if (and spn (loc<=? pos (span-start spn)))
         (cons cursor elems)
         (cons head (insert-cursor-in-list rest pos cursor)))]))

;; insert-cursor-at : Stx Loc -> Stx
;; Inserts a cursor identifier at the given position in the syntax tree.
;; Finds the appropriate list position and inserts the cursor there.
;; For lists/pairs, position must be strictly after start.
(define (insert-cursor-at root pos cursor)
  (let loop ([syn root])
    (match syn
      [(stx e spn marks)
       (cond
         ;; Empty list node - insert cursor here if position is nearby
         [(and (null? e) spn
               (loc<=? (span-start spn) pos)
               (loc<=? pos (span-end spn)))
          (stx (list cursor) spn marks)]
         ;; Non-empty list: position must be strictly after start and before end
         [(and spn (list? e) (not (null? e))
               (loc<? (span-start spn) pos)
               (loc<? pos (span-end spn)))
          (define new-elems (map loop e))
          (if (equal? new-elems e)
              ;; No child changed, insert cursor at the right position
              (stx (insert-cursor-in-list e pos cursor) spn marks)
              (stx new-elems spn marks))]
         [(and (list? e) (not (null? e)))
          (define new-elems (map loop e))
          (if (equal? new-elems e) syn (stx new-elems spn marks))]
         ;; Dotted pair: position must be strictly after start and before end
         [(and spn (pair? e)
               (loc<? (span-start spn) pos)
               (loc<? pos (span-end spn)))
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

;; replace-node-with-cursor : Stx Stx Stx -> Stx
;; Replaces the target node with the cursor in the syntax tree.
;; Uses object identity (eq?) to find the target.
(define (replace-node-with-cursor root target cursor)
  (let loop ([syn root])
    (if (eq? syn target)
        cursor
        (match syn
          [(stx e spn marks)
           (cond
             [(list? e)
              (define new-elems (map loop e))
              (if (equal? new-elems e) syn (stx new-elems spn marks))]
             [(pair? e)
              (define new-e (cons (loop (car e)) (loop (cdr e))))
              (if (equal? new-e e) syn (stx new-e spn marks))]
             [else syn])]
          [_ syn]))))

;; ============================================================
;; Tests
;; ============================================================

(module+ test
  (require rackunit)
  (require "reader.rkt")

  ;; cursor-identifier? tests
  (let* ([zero-loc (loc "test.tsn" 0 5)]
         [zero-span (span zero-loc zero-loc)]
         [nonzero-span (span (loc "test.tsn" 0 5) (loc "test.tsn" 0 10))])
    ;; A cursor produced by make-cursor satisfies cursor-identifier?
    (check-true (cursor-identifier? (make-cursor zero-loc))
                "make-cursor produces a cursor-identifier?")
    ;; An interned symbol named "cursor123" with zero-width span is NOT a cursor
    (check-false (cursor-identifier? (stx 'cursor123 zero-span '()))
                 "interned symbol starting with cursor is not a cursor")
    ;; An uninterned gensym starting with "cursor" but with a real span is NOT a cursor
    (check-false (cursor-identifier? (stx (racket-gensym 'cursor) nonzero-span '()))
                 "cursor gensym with non-zero-width span is not a cursor")
    ;; An uninterned gensym NOT starting with "cursor" with a zero-width span is NOT a cursor
    (check-false (cursor-identifier? (stx (racket-gensym 'foo) zero-span '()))
                 "uninterned gensym not starting with cursor is not a cursor")
    ;; A non-identifier (number) is not a cursor
    (check-false (cursor-identifier? (stx 42 zero-span '()))
                 "non-identifier stx is not a cursor"))

  ;; insert-or-replace-cursor tests
  (test-case "insert-or-replace-cursor: on identifier"
    ;; When cursor is on an identifier, it should be replaced
    (define source "(define x 1)\nx")
    (define syns (string->stxs "test" source))
    (define pos (loc "test" 1 0))  ; On the 'x' at line 1
    (define cursor (make-cursor pos))
    (define with-cursor (insert-or-replace-cursor syns pos cursor))
    ;; Should have 2 forms, second should be the cursor
    (check-equal? (length with-cursor) 2)
    (check-true (cursor-identifier? (second with-cursor))))

  (test-case "insert-or-replace-cursor: after all forms"
    ;; When cursor is after all forms, insert as new top-level form
    (define source "(define x 1)\n(define y 2)")
    (define syns (string->stxs "test" source))
    (define pos (loc "test" 1 12))  ; After closing paren of second form
    (define cursor (make-cursor pos))
    (define with-cursor (insert-or-replace-cursor syns pos cursor))
    ;; Should have 3 forms (2 defines + cursor)
    (check-equal? (length with-cursor) 3)
    (check-true (cursor-identifier? (third with-cursor))))

  (test-case "insert-or-replace-cursor: between forms"
    ;; When cursor is between forms, insert at that position
    (define source "(define x 1)\n\n(define y 2)")
    (define syns (string->stxs "test" source))
    (define pos (loc "test" 1 0))  ; On the empty line
    (define cursor (make-cursor pos))
    (define with-cursor (insert-or-replace-cursor syns pos cursor))
    ;; Should have 3 forms: first define, cursor, second define
    (check-equal? (length with-cursor) 3)
    (check-true (cursor-identifier? (second with-cursor))))

  (test-case "insert-or-replace-cursor: at start before all forms"
    ;; When cursor is before all forms, insert at the beginning
    (define source "(define x 1)")
    (define syns (string->stxs "test" source))
    (define pos (loc "test" 0 0))  ; At opening paren
    (define cursor (make-cursor pos))
    (define with-cursor (insert-or-replace-cursor syns pos cursor))
    ;; Should have 2 forms: cursor, then define
    (check-equal? (length with-cursor) 2)
    (check-true (cursor-identifier? (first with-cursor))))

  (test-case "insert-or-replace-cursor: identifier inside a list"
    ;; When cursor is on an identifier inside a list, replace it
    (define source "(let ([x 1]) x)")
    (define syns (string->stxs "test" source))
    (define pos (loc "test" 0 13))  ; On the 'x' in the body
    (define cursor (make-cursor pos))
    (define with-cursor (insert-or-replace-cursor syns pos cursor))
    ;; Should have the let with cursor replacing the x in the body
    (check-match with-cursor
                 (list (stx-quote (let ([x 1]) ,(? cursor-identifier?))))))

  (test-case "insert-or-replace-cursor: insert inside a list"
    ;; When cursor is inside a list but not on an identifier, insert it
    (define source "(let () )")
    (define syns (string->stxs "test" source))
    (define pos (loc "test" 0 8))  ; Inside the empty body, before closing paren
    (define cursor (make-cursor pos))
    (define with-cursor (insert-or-replace-cursor syns pos cursor))
    ;; Should have the let with cursor inserted in the body
    (check-match with-cursor
                 (list (stx-quote (let () ,(? cursor-identifier?))))))

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

  ;; fault-tolerant block: unbound call in definition context treated as expression
  (check-match
   (expand
    '(block
      (bad)
      (define x 2)))
   `(block
     (#%expression ,(? stx-error?))
     (define x0 2)))

  ;; bare identifier in block - treated as implicit #%expression
  (check-match
   (expand '(block x))
   `(block (#%expression ,(? stx-error?))))

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
           (#%expression 3)))
  ;; optimistic sub-expression expansion
  (check-match
   (expand
    '(let ([]) (let ([x 1]) x)))
   `(let ,(? stx-error?) (let ([x0 1]) x0)))
  (check-match
   (expand
    '(let ([42 (let ([x 1]) x)]) (let ([x 1]) x)))
   `(let ([,(? stx-error?) (let ([x0 1]) x0)]) (let ([x1 1]) x1)))
  (check-equal?
   (expand
    '(let-syntax () (let ([x 1]) x)))
   '(let ([x0 1]) x0))
  (check-equal?
   (expand
    '(let-syntax ([m (syntax-rules () [(m (~var x expr)) x])]) (m 1)))
   '1)
  ;; x:expr is shorthand for (~var x expr)
  (check-equal?
   (expand
    '(let-syntax ([m (syntax-rules () [(m x:expr) x])]) (m 1)))
   '1)
  (test-case
   "unknown syntax class is reported once and still binds the pattern variable"
   (define sexp '(let-syntax ([m (syntax-rules () [(m x:foo) x])]) (m 1)))
   (define result (analyze! (list (sexpr->syntax sexp))))
   (check-equal? (length (expander-result-errors result)) 1)
   (check-equal? (expander-result-expanded result) '(block (#%expression 1))))
  (test-case
   "a datum literal whose name contains a colon is not an annotation"
   (define sexp '(let-syntax ([m (syntax-rules (a:b) [(m a:b) 1] [(m x) 2])]) (m a:b)))
   (define result (analyze! (list (sexpr->syntax sexp))))
   (check-equal? (expander-result-errors result) (list))
   (check-equal? (expander-result-expanded result) '(block (#%expression 1))))
  (test-case
   "a datum literal whose name contains a colon does not match a non-literal"
   ;; Without the literal guard, a:b would match anything as an annotated pattern
   ;; variable and the first clause would win.
   (define sexp '(let-syntax ([m (syntax-rules (a:b) [(m a:b) 1] [(m x) 2])]) (m 5)))
   (define result (analyze! (list (sexpr->syntax sexp))))
   (check-equal? (expander-result-errors result) (list))
   (check-equal? (expander-result-expanded result) '(block (#%expression 2))))
  (test-case
   "an identifier with nothing on one side of its colon is an ordinary pattern variable"
   (define sexp '(let-syntax ([m (syntax-rules () [(m x:) x:])]) (m 1)))
   (define result (analyze! (list (sexpr->syntax sexp))))
   (check-equal? (expander-result-errors result) (list))
   (check-equal? (expander-result-expanded result) '(block (#%expression 1))))
  (test-case
   "OSE doesn't happen when there is not a failure"
   (define sexp
     '(block
       (define-syntax my-let
         (syntax-rules ()
           [(my-let ([x (~var e expr)]) (~var b expr)) (let ([x e]) b)]))
       (my-let ([x 1]) x)))
   (define result (analyze! (list (sexpr->syntax sexp))))
   (check-equal? (expander-result-errors result) (list)))
  (test-case
   "delayed OSE in a definition context is reported exactly once"
   ;; The failing match is recorded when it is raised in pass 1; running the
   ;; carried subexpressions in pass 2 must not record a second diagnostic.
   (define sexp
     '(block
       (define-syntax m
         (syntax-rules ()
           [(m 1 (~var e expr)) 1]))
       (m 2 (let ([q 1]) q))
       (define x 2)
       ;; the trailing expression keeps this block well-formed; the definition
       ;; after the macro call is what makes pass 2 the earliest safe time to
       ;; expand the carried subexpressions
       x))
   (define result (analyze! (list (sexpr->syntax sexp))))
   (check-equal? (length (expander-result-errors result)) 1))

  ;; ----------------------------------------
  ;; Block tail rule
  ;; ----------------------------------------

  (test-case
   "a block ending in an expression is well-formed"
   (define result (analyze! (list (sexpr->syntax '(block (define x 1) x)))))
   (check-equal? (expander-result-errors result) (list)))

  (test-case
   "a block ending in a definition is an error"
   (define result (analyze! (list (sexpr->syntax '(block 1 (define x 1))))))
   (check-equal? (map stx-error-message (expander-result-errors result))
                 (list "block must end in an expression")))

  (test-case
   "an empty block is an error"
   (define result (analyze! (list (sexpr->syntax '(block)))))
   (check-equal? (map stx-error-message (expander-result-errors result))
                 (list "block must end in an expression")))

  (test-case
   "a block ending in a begin that ends in an expression is well-formed"
   (define result (analyze! (list (sexpr->syntax '(block (begin (define x 1) x))))))
   (check-equal? (expander-result-errors result) (list)))

  (test-case
   "a block ending in a begin that ends in a definition is an error"
   (define result (analyze! (list (sexpr->syntax '(block 1 (begin (define x 1)))))))
   (check-equal? (map stx-error-message (expander-result-errors result))
                 (list "block must end in an expression")))

  (test-case
   "the tail is checked after expansion: a macro expanding to a definition is an error"
   ;; the surface tail is a macro call, so only the expanded form knows whether
   ;; this block ends in an expression
   (define sexp '(block (define-syntax m (syntax-rules () [(m) (define y 1)])) (m)))
   (define result (analyze! (list (sexpr->syntax sexp))))
   (check-equal? (map stx-error-message (expander-result-errors result))
                 (list "block must end in an expression")))

  (test-case
   "the tail is checked after expansion: a macro expanding to an expression is well-formed"
   (define sexp '(block (define-syntax m (syntax-rules () [(m) 1])) (m)))
   (define result (analyze! (list (sexpr->syntax sexp))))
   (check-equal? (expander-result-errors result) (list)))

  (test-case
   "a broken tail is not reported twice"
   ;; the tail is already an error node; adding a block-tail error would blame
   ;; the same form a second time
   (define result (analyze! (list (sexpr->syntax '(block (define x 1) (define))))))
   (check-equal? (length (expander-result-errors result)) 1))

  (test-case
   "top-level forms are a module body, not a block: definitions may come last"
   (define result (analyze! (list (sexpr->syntax '(define x 1)))))
   (check-equal? (expander-result-errors result) (list)))

  (test-case
   "an empty program is well-formed"
   (define result (analyze! (list)))
   (check-equal? (expander-result-errors result) (list)))

  ;; ----------------------------------------
  ;; Errors embedded in the expanded output are recorded
  ;; ----------------------------------------

  (test-case
   "a variable in head position is recorded"
   (define result (analyze! (list (sexpr->syntax '(block (define f 1) (f 2))))))
   (check-equal? (map stx-error-message (expander-result-errors result))
                 (list "not a procedure or syntax")))

  (test-case
   "a non-identifier in head position is recorded"
   (define result (analyze! (list (sexpr->syntax '((1 2))))))
   (check-equal? (map stx-error-message (expander-result-errors result))
                 (list "not a procedure or syntax")))

  (test-case
   "a macro used as a variable reference is recorded"
   (define sexp '(let-syntax ([m (syntax-rules () [(m) 1])]) m))
   (define result (analyze! (list (sexpr->syntax sexp))))
   (check-equal? (map stx-error-message (expander-result-errors result))
                 (list "unexpected binding type")))

  ;; ----------------------------------------
  ;; Ellipsis tests
  ;; ----------------------------------------

  ;; basic: zero elements
  (check-equal?
   (expand
    '(let-syntax ([m (syntax-rules () [(m x ...) (block x ...)])])
       (m)))
   '(block))

  ;; basic: single pvar ellipsis expands all args
  (check-equal?
   (expand
    '(let-syntax ([my-list (syntax-rules () [(my-list x ...) (block x ...)])])
       (my-list 1 2 3)))
   '(block (#%expression 1) (#%expression 2) (#%expression 3)))

  ;; mixed depth: a is depth-0, b is depth-1
  (check-equal?
   (expand
    '(let-syntax ([rep (syntax-rules () [(rep a b ...) (block (block a b) ...)])])
       (rep 1 2 3 4)))
   '(block (#%expression (block (#%expression 1) (#%expression 2)))
           (#%expression (block (#%expression 1) (#%expression 3)))
           (#%expression (block (#%expression 1) (#%expression 4)))))

  ;; structured inner pattern: ([x e] ...) with template using both pvars
  ;; gensym names vary so we capture and compare: the binding and reference must match
  (check-match
   (expand
    '(let-syntax ([my-let* (syntax-rules ()
                              [(my-let* ([x e] ...) body)
                               (block (define x e) ... body)])])
       (my-let* ([a 1] [b 2] [c 3]) a)))
   `(block (define ,a 1) (define ,_ 2) (define ,_ 3) (#%expression ,a-ref))
   (equal? a a-ref))

  ;; nested ellipsis: ((a ...) ...)
  (check-equal?
   (expand
    '(let-syntax ([nested (syntax-rules ()
                             [(nested (a ...) ...) (block (block a ...) ...)])])
       (nested (1 2) (3 4 5) ())))
   '(block (#%expression (block (#%expression 1) (#%expression 2)))
           (#%expression (block (#%expression 3) (#%expression 4) (#%expression 5)))
           (#%expression (block))))

  ;; leading fixed + trailing ellipsis in pattern: (first rest ...)
  ;; demonstrates that ellipsis at end of list works via pair-case recursion on (rest ...)
  (check-equal?
   (expand
    '(let-syntax ([m (syntax-rules () [(m first rest ...) (block first rest ...)])])
       (m 10 20 30)))
   '(block (#%expression 10) (#%expression 20) (#%expression 30)))

  ;; template: content after the ellipsis — (x ... sentinel)
  ;; demonstrates that ... need not be the last element in a template list
  (check-equal?
   (expand
    '(let-syntax ([m (syntax-rules () [(m x ...) (block x ... 99)])])
       (m 1 2 3)))
   '(block (#%expression 1) (#%expression 2) (#%expression 3) (#%expression 99)))
  )
