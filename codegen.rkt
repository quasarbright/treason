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
         (for-template racket/base)
         "stx.rkt")

;; ============================================================
;; Data Definitions
;; ============================================================

;; A Context is a (context [Symbol -> Identifier] [XSExpr -> (or/c srcloc #f)])
(struct context [variable origin])
;; variable : the Racket identifier standing for a renamed variable
;; origin : where in the program an expanded node came from, if known

;; ============================================================
;; Compiling
;; ============================================================

;; xsexpr->module-body : XSExpr Syntax
;;                       #:renamings [HasheqOf Symbol Symbol]
;;                       #:origins [HasheqOf XSExpr Span]
;;                       -> [Listof Syntax]
;; Compiles an expanded program into the forms of a Racket module body.
;; The top-level block is a module body rather than an expression, so its
;; definitions become module-level definitions.
;;
;; A variable is named as the program names it, given by renamings, so that
;; error messages and procedure names read the way the program does. Two
;; variables the program names alike, which hygiene has told apart, stay apart
;; because each renaming gets a scope of its own. ctx gives every variable the
;; lexical context of the module being compiled, so that the definitions belong
;; to it; nothing from Racket is bound there (the language provides only
;; #%module-begin), so a variable cannot clash with a Racket binding of the same
;; name. Without a renaming, a variable is named by its renamed symbol.
;;
;; Compiled forms carry the source location of the syntax they came from, given
;; by origins, so that tools reporting a runtime error can point at the program.
(define (xsexpr->module-body program ctx
                             #:renamings [renamings (hasheq)]
                             #:origins [origins (hasheq)])
  (define scopes (make-hasheq))
  (define (variable name)
    (define introduce (hash-ref! scopes name make-syntax-introducer))
    (introduce (datum->syntax ctx (hash-ref renamings name name))))
  (define (origin node)
    (define spn (hash-ref origins node #f))
    (and spn (span->srcloc spn)))
  (define cx (context variable origin))
  (match program
    [`(block ,defs ...)
     (for/list ([def defs]) (compile-def def cx))]))

;; compile-def : XDef Context -> Syntax
;; Compiles a definition-context form. The same Racket forms serve at module
;; level and inside a block body.
(define (compile-def def cx)
  (match def
    [`(define ,name ,expr)
     #`(define #,((context-variable cx) name) #,(compile-expr expr cx))]
    [`(begin ,defs ...)
     #`(begin #,@(for/list ([d defs]) (compile-def d cx)))]
    [`(#%expression ,expr)
     (compile-expr expr cx)]
    [_ (unknown-form 'compile-def def)]))

;; compile-expr : XSExpr Context -> Syntax
;; Compiles an expression, located where the program has it.
;; A block becomes an internal-definition context, which the expander has
;; already checked ends in an expression.
(define (compile-expr expr cx)
  (define (variable name) ((context-variable cx) name))
  (located
   cx expr
   (match expr
     [(or (? number?) (? boolean?))
      #`(quote #,expr)]
     [(? symbol?)
      (variable expr)]
     [`(block ,defs ...)
      #`(let () #,@(for/list ([d defs]) (compile-def d cx)))]
     [`(let ([,name ,rhs]) ,body)
      #`(let ([#,(variable name) #,(compile-expr rhs cx)])
          #,(compile-expr body cx))]
     [`(#%expression ,e)
      (compile-expr e cx)]
     [`(lambda (,names ...) ,body)
      #`(lambda #,(for/list ([name names]) (variable name))
          #,(compile-expr body cx))]
     [`(if ,test ,then ,alt)
      #`(if #,(compile-expr test cx) #,(compile-expr then cx) #,(compile-expr alt cx))]
     [`(#%app ,operator ,args ...)
      #`(#%app #,(compile-expr operator cx)
               #,@(for/list ([arg args]) (compile-expr arg cx)))]
     [`(#%primitive ,name)
      ;; this module's own context, where racket/base is bound for the code it
      ;; generates, so the name refers to racket/base's binding of it
      (datum->syntax #'here name)]
     [_ (unknown-form 'compile-expr expr)])))

;; located : Context XSExpr Syntax -> Syntax
;; Gives compiled syntax the source location of the expanded node it came from,
;; when that is known.
(define (located cx node compiled)
  (define srcloc ((context-origin cx) node))
  (if srcloc
      (datum->syntax compiled (syntax-e compiled) srcloc compiled)
      compiled))

;; unknown-form : Symbol Any -> Nothing
;; Reports a form this compiler has no translation for.
(define (unknown-form who form)
  (error who "internal error: no translation for expanded form ~s" form))

;; ============================================================
;; Tests
;; ============================================================

(module+ test
  (require rackunit
           racket/list
           "stx.rkt"
           (only-in "expander.rkt" primitives))

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
   "a lambda keeps its parameters"
   (check-equal? (compile '(block (#%expression (lambda (x0 y1) x0))))
                 '((lambda (x0 y1) x0))))

  (test-case
   "if keeps its condition and branches"
   (check-equal? (compile '(block (#%expression (if #t 1 2))))
                 '((if '#t '1 '2))))

  (test-case
   "an application applies its operator to its arguments"
   (check-equal? (compile '(block (define f0 1) (#%expression (#%app f0 2))))
                 '((define f0 '1) (#%app f0 '2))))

  (test-case
   "a primitive becomes the racket/base binding of the same name"
   (define plus (car (xsexpr->module-body '(block (#%expression (#%primitive +))) #'here)))
   (check-equal? (syntax->datum plus) '+)
   (check-true (free-template-identifier=? plus #'+)))

  (test-case
   "every primitive the expander binds exists in racket/base"
   ;; a name racket/base does not provide would compile to an unbound
   ;; identifier: a static error from Racket, after treason's checks passed
   (for ([name primitives])
     (define id (car (xsexpr->module-body `(block (#%expression (#%primitive ,name))) #'here)))
     (check-not-false (identifier-template-binding id)
                      (format "~a is not bound in racket/base" name))))

  (test-case
   "a variable is named as the program names it"
   (define body (xsexpr->module-body '(block (define x0 1) (#%expression x0)) #'here
                                     #:renamings (make-hasheq '((x0 . x)))))
   (check-equal? (map syntax->datum body) '((define x '1) x)))

  (test-case
   "two variables the program names alike stay distinct"
   ;; what hygiene told apart must stay apart: each renaming gets its own scope
   (define body (xsexpr->module-body '(block (define t0 5) (#%expression (let ([t1 1]) t0)))
                                     #'here
                                     #:renamings (make-hasheq '((t0 . t) (t1 . t)))))
   (check-equal? (map syntax->datum body) '((define t '5) (let ([t '1]) t)))
   (define defined (cadr (syntax->list (first body))))
   (define bound (car (syntax->list (cadr (syntax->list (second body))))))
   (define bound-id (car (syntax->list bound)))
   (define referenced (caddr (syntax->list (second body))))
   (check-true (bound-identifier=? defined referenced))
   (check-false (bound-identifier=? bound-id referenced)))

  (test-case
   "compiled code carries the source location of the syntax it came from"
   (define app '(#%app (#%primitive +) 1 2))
   (define spn (span (loc "p.tsn" 2 4 20) (loc "p.tsn" 2 13 29)))
   (define body (xsexpr->module-body `(block (#%expression ,app)) #'here
                                     #:origins (make-hasheq (list (cons app spn)))))
   (check-equal? (list (syntax-source (first body)) (syntax-line (first body))
                       (syntax-column (first body)) (syntax-position (first body))
                       (syntax-span (first body)))
                 (list "p.tsn" 3 4 21 9)))

  (test-case
   "a form with no translation is an internal error, not bad Racket"
   (check-exn #rx"internal error"
              (lambda () (compile '(block (#%expression (set! x0 1))))))))
