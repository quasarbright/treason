#lang racket/base

;; Compiling treason's expanded output to Racket.
;;
;; The expander has already done the hard part: hygiene is resolved, and every
;; variable is renamed to a name no other binding shares. What is left is a
;; small, fixed set of forms that each have a direct Racket counterpart, so
;; compiling is a structural translation.
;;
;; This only ever sees a program the expander accepted without errors; the
;; language reports errors before calling it. A form it does not recognise is
;; therefore a bug here, not in the user's program, and fails loudly rather than
;; producing code Racket would reject.

(provide xsexpr->module-body)

(require racket/match
         (for-template racket/base))

;; ============================================================
;; Compiling
;; ============================================================

;; xsexpr->module-body : XSExpr Syntax -> [Listof Syntax]
;; Compiles an expanded program into the forms of a Racket module body.
;; ctx gives treason's variables the lexical context of the module being
;; compiled, so that the definitions they name belong to it. Nothing from Racket
;; is bound in that context (the language provides only #%module-begin), and the
;; expander has renamed every variable apart, so a variable's name cannot clash
;; with anything else there, even one like add1 that racket/base also binds.
;; The top-level block is a module body rather than an expression, so its
;; definitions become module-level definitions.
(define (xsexpr->module-body program ctx)
  (match program
    [`(block ,defs ...)
     (for/list ([def defs]) (compile-def def ctx))]))

;; compile-def : XDef Syntax -> Syntax
;; Compiles a definition-context form. The same Racket forms serve at module
;; level and inside a block body.
(define (compile-def def ctx)
  (match def
    [`(define ,name ,expr)
     #`(define #,(datum->syntax ctx name) #,(compile-expr expr ctx))]
    [`(begin ,defs ...)
     #`(begin #,@(for/list ([d defs]) (compile-def d ctx)))]
    [`(#%expression ,expr)
     (compile-expr expr ctx)]
    [_ (unknown-form 'compile-def def)]))

;; compile-expr : XSExpr Syntax -> Syntax
;; Compiles an expression.
;; A block becomes an internal-definition context, which the expander has
;; already checked ends in an expression.
(define (compile-expr expr ctx)
  (match expr
    [(or (? number?) (? boolean?))
     #`(quote #,expr)]
    [(? symbol?)
     (datum->syntax ctx expr)]
    [`(block ,defs ...)
     #`(let () #,@(for/list ([d defs]) (compile-def d ctx)))]
    [`(let ([,name ,rhs]) ,body)
     #`(let ([#,(datum->syntax ctx name) #,(compile-expr rhs ctx)])
         #,(compile-expr body ctx))]
    [`(#%expression ,e)
     (compile-expr e ctx)]
    [_ (unknown-form 'compile-expr expr)]))

;; unknown-form : Symbol Any -> Nothing
;; Reports a form this compiler has no translation for.
(define (unknown-form who form)
  (error who "internal error: no translation for expanded form ~s" form))

;; ============================================================
;; Tests
;; ============================================================

(module+ test
  (require rackunit)

  ;; compile : XSExpr -> [Listof S-Expression]
  ;; The compiled module body, as data.
  (define (compile program)
    (map syntax->datum (xsexpr->module-body program #'here)))

  (test-case
   "top-level definitions become module-level definitions"
   (check-equal? (compile '(block (define x0 1) (#%expression x0)))
                 '((define x0 '1) x0)))

  (test-case
   "a nested block becomes an internal-definition context"
   (check-equal? (compile '(block (#%expression (block (define y1 2) (#%expression y1)))))
                 '((let () (define y1 '2) y1))))

  (test-case
   "let binds one variable around its body"
   (check-equal? (compile '(block (#%expression (let ([x0 1]) x0))))
                 '((let ([x0 '1]) x0))))

  (test-case
   "a begin keeps its definitions, including an empty one from define-syntax"
   (check-equal? (compile '(block (begin) (begin (define x0 #t)) (#%expression x0)))
                 '((begin) (begin (define x0 '#t)) x0)))

  (test-case
   "a form with no translation is an internal error, not bad Racket"
   (check-exn #rx"internal error"
              (lambda () (compile '(block (#%expression (lambda (x) x))))))))
