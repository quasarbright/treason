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

(define gensym-ctr (make-parameter #f))

(define (gensym x)
  (define ctr (gensym-ctr))
  (gensym-ctr (+ ctr 1))
  (string->symbol (format "~a~a" (symbol->string x) ctr)))

(define (ext-env* penv pvar* ustx*)
  (for/fold ([penv penv])
            ([ustx ustx*] [pvar pvar*])
    (hash-set penv pvar ustx)))

(define keywords '(let let-syntax syntax-rules define
                    define-syntax block begin #%expression))

(struct macro-closure [macrot scp])

(struct identifier [symbol marks] #:transparent)

(define (mark-id id mark)
  (match id
    [(identifier sym marks)
     (identifier sym (cons mark marks))]))

(define (mark-syntax stx mark)
  (match stx
    [(cons a d)
     (cons (mark-syntax a mark) (mark-syntax d mark))]
    [(? identifier? id)
     (mark-id id mark)]
    [_ stx]))

(define (top-mark=? id mark)
  (match id
    [(identifier sym (cons (== mark) _))
     #t]
    [_ #f]))

(define (drop-top-mark id)
  (match id
    [(identifier sym (cons top-mark marks-rest))
     (identifier sym marks-rest)]))

(define (fresh-def-mark) (gensym 'd))
(define (fresh-use-mark) (gensym 'u))

(struct core-scope [bindings])
(struct scope [parent bindings])
(struct disjoin [mark1 scope1 mark2 scope2])

(struct unbound [])

(define (new-scope parent)
  (scope parent (make-hash)))

;; Scope, Identifier -> (or Binding Unbound)
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

;; Scope, Identifier, Binding -> Void
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
       [(top-mark=? id mark1) (scope-bind! scope1 (drop-top-mark id) bnd)]
       [(top-mark=? id mark2) (scope-bind! scope2 (drop-top-mark id) bnd)]
       [else (error 'scope-bind "malformed environment")])]))

(define initial-scope
  (core-scope
   (for/fold ([acc (hash)])
             ([sym keywords])
     (hash-set acc (identifier sym '()) sym))))

;; Syntax, Scope -> Syntax
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

(define (expand-def*-pass1 def* scp)
  (for/list ([def def*])
    (expand-def-pass1 def scp)))

(define (expand-def*-pass2 def* scp)
  (for/list ([def def*])
    (expand-def-pass2 def scp)))

;; When a macro expands in the first pass of definition
;; context expansion, continued expansion in the second
;; pass needs to use the disjoin scope. The result of
;; pass 1 expansion may include this structure in order
;; to remember that scope for pass 2.
(struct with-disjoin [stx scp])

;; Syntax, Scope -> Syntax
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
     (with-disjoin (expand-def-pass1 marked-stx disjoined-scp) disjoined-scp)]))

;; Syntax, Scope -> Syntax
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

;; Identifier, Syntax, Scope -> (values Syntax Scope)
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
  (define introduced-defn-scp (new-scope def-scp))
  (define disjoined-scp (disjoin def-mark introduced-defn-scp
                                 use-mark use-scp))
  (values marked-stx disjoined-scp))

(define (select-syntax-rule macrot expr def-scp use-scp)
  (match macrot
    [`(,syntax-rules-id (,literal-id* ...)
                        ,clause* ...)
     (define is-literal? (make-is-literal? literal-id*))
     (define literal-match? (make-literal-match? def-scp use-scp))
     (try-clauses clause* expr is-literal? literal-match?)]))

(define (make-is-literal? literal-id*)
  (lambda (id)
    (memf (lambda (x) (bound-identifier=? id x)) literal-id*)))

;; Identifier -> Identifier -> Boolean
(define (bound-identifier=? id1 id2)
  (and (eq? (identifier-symbol id1) (identifier-symbol id2))
       (equal? (identifier-marks id1) (identifier-marks id2))))

;; Scope, Scope -> (Identifier, Identifier -> Boolean)
(define (make-literal-match? def-scp use-scp)
  (lambda (literal-id target-id)
    (define literal-binding (scope-resolve def-scp literal-id))
    (define target-binding (scope-resolve use-scp target-id))
    (or (and (not (unbound? literal-binding)) (not (unbound? target-binding))
             (eq? literal-binding target-binding))
        (and (unbound? literal-binding) (unbound? target-binding)
             (eq? (identifier-symbol literal-id) (identifier-symbol target-id))))))

(define (try-clauses clauses expr is-literal? literal-match?)
  (match clauses
    [(cons `[,pat ,tmpl] rest)
     (define maybe-penv (match-top-pattern pat expr is-literal? literal-match?))
     (if maybe-penv
         (values maybe-penv tmpl)
         (try-clauses rest expr is-literal? literal-match?))]
    ['() (error 'syntax-rules "no pattern matched")]))

;; This implementation of syntax-rules matching omits ellipses (as well as datatypes
;; not supported in the input language, such as vectors and boolean literals)
(define (match-top-pattern pat expr is-literal? literal-match?)
  ;; The `car` of the `expr` is the macro name, and in syntax-rules
  ;; the `car` of the `pat` stands for the macro name and is ignored.
  (match-pattern (cdr pat) (cdr expr) is-literal? literal-match?))

(define (match-pattern pat expr is-literal? literal-match?)
  (match* (pat expr)
    ;; literal-id
    [((? identifier? lit) (? identifier? target-id))
     #:when (is-literal? lit)
     (and (literal-match? lit target-id)
          (hash))]
    ;; pvar
    [((? identifier? pvar) stx)
     #:when (not (is-literal? pvar))
     (hash pvar stx)]
    ;; datum literal
    [((? number? v) v)
     (hash)]
    ;; cons
    [(`(,pa . ,pd) `(,ea . ,ed))
     (let ([resa (match-pattern pa ea is-literal? literal-match?)])
       (and resa
            (let ([resd (match-pattern pd ed is-literal? literal-match?)])
              (and resd
                   (hash-union resa resd)))))]
    [('() '()) (hash)]
    [(_ _) #f]))

(define (project-def tmpl penv)
  (match tmpl
    [(cons a d)
     (cons (project-def a penv) (project-def d penv))]
    [(? identifier? pvar)
     #:when (hash-has-key? penv pvar)
     '_]
    [_ tmpl]))

(define (project-use tmpl penv)
  (match tmpl
    [(? identifier? pvar)
     #:when (hash-has-key? penv pvar)
     (hash-ref penv pvar)]
    [(cons a d)
     (cons (project-use a penv) (project-use d penv))]
    ['() '()]
    [_ '_]))

(define (combine-projections proj1 proj2)
  (match* (proj1 proj2)
    [('_ proj2) proj2]
    [(proj1 '_) proj1]
    [(same same) same]
    [((cons proj1a proj1d) (cons proj2a proj2d))
     (cons (combine-projections proj1a proj2a)
           (combine-projections proj1d proj2d))]))

(define (sexpr->syntax s)
  (match s
    [(cons a d)
     (cons (sexpr->syntax a) (sexpr->syntax d))]
    ['_ '_]
    [(? symbol? s) (symbol->id s)]
    [_ s]))

(define (symbol->id s)
  (identifier s (list)))

;; SExpression -> SExpression
(define (expand e)
  (parameterize ([gensym-ctr 0])
    (expand-expr (sexpr->syntax e) initial-scope)))

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