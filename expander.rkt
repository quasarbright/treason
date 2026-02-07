#lang racket

;; unhygienic macro expander

;; use-site stx keeps its span. macro-introduced stx gets template span.

;; TODO errors with source info
;; TODO handle errors by reporting diagnostics
;; TODO for macros, should we set the emitted overall span to the whole use's span?
;; otherwise it will point to the template location
;; however, if the emitted syntax is just use-site syntax, we don't want this.
;; we could get around this by setting the span to #f from the transformer (but only on the outermost)
;; in the usual case and using or in the expander on the span

#|
(add/macro #t #f)
~>
(add/proc #t #f) ; runtime error. should point to use-site, not template. this can be done with runtime contracts in macro definitions
|#

(provide (all-defined-out))
(require "stx.rkt"
         "stx-quote.rkt"
         racket/hash)

;; data definitions

;; a Transformer is a
;; (stx? -> stx?)
;; which represents a stx transformer from a let-syntax-rule macro definition


;; A StxError is a
(struct stx-error [who message expr sub-expr] #:transparent)
;; where
;; who is a symbol or #f
;; message is a string
;; expr is the stx? with an error in it
;; sub-expr is the stx? that caused the error, or #f if it's just expr
;; For example: in (let ([123 4]) body),
;; the whole let would be the expr and the 123 would be sub-expr

;; An ExpanderResult is a
(define expander-result? (or/c stx? stx-error?))
;; could also be a stx? with stx-error? children

;; stx? [(hash symbol? (or/c #f (stx? -> stx?)))] -> ExpanderResult
;; env maps symbols to transformers or #f (for normal var)
(define (expand syn [env (hash)])
  (match syn
    [(stx-quote (let ([,x ,rhs]) ,body))
     (define x-sym (stx-e x))
     (cond
       [(symbol? x-sym)
        (define rhs^ (expand rhs env))
        (define body^ (expand body (hash-set env x-sym #f)))
        (stx-rebuild syn (let ([,x ,rhs^]) ,body^))]
       [else 
        (define err (stx-error 'let "expected a variable on the left-hand-side of binding pair" syn x))
        (define rhs^ (expand rhs env))
        ;; body may get unbound x, but that's ok
        (define body^ (expand body env))
        (stx-rebuild syn (let ([,err ,rhs^]) ,body^))])]
    [(stx-quote (if ,cnd ,thn ,els))
     (define cnd^ (expand cnd env))
     (define thn^ (expand thn env))
     (define els^ (expand els env))
     (stx-rebuild syn (if ,cnd^ ,thn^ ,els^))]
    [(stx-quote (let-syntax-rule ([,pattern ,template]) ,body))
     (match (stx-e pattern)
       [(cons (stx (? symbol? m-sym) _) _)
        (define transformer (make-transformer m-sym pattern template))
        (expand body (hash-set env m-sym transformer))]
       [_
        (define err (stx-error 'let-syntax-rule "pattern must be a list with an identifier head" syn pattern))
        ;; body may get unbound m, but that's ok
        (define body^ (expand body env))
        (stx-rebuild syn (let-syntax-rule ([,err ,template]) ,body^))])]
    [(stx (or (and who (or 'let 'if 'let-syntax-rule))
              (cons (stx (and who (or 'let 'if 'let-syntax-rule)) _) _)) _)
     (stx-error who "bad syntax" syn #f)]
    [(stx (cons m _) _)
     ;; (macro) application
     (with-stx-error-handling
       (define m-sym (stx-e m))
       (unless (symbol? m-sym)
         (raise (stx-error #f "application head must be an identifier" syn m)))
       (define transformer (hash-ref env m-sym (lambda () (raise (stx-error m-sym "unbound var" syn m)))))
       ;; no function calls, only macro calls
       (unless transformer
         (raise (stx-error m-sym "application head not a macro" syn m)))
       ;; note, stx-error from transformer is handled since we're in a with-stx-error-handling
       (define syn^ (transformer syn))
       (expand syn^ env))]
    [datum-syn
     (with-stx-error-handling
       (define x-sym (stx-e datum-syn))
       (when (symbol? x-sym)
         (define transformer (hash-ref env x-sym (lambda () (raise (stx-error x-sym "unbound var" syn #f)))))
         (when transformer
           (raise (stx-error x-sym "bad syntax" syn #f))))
       datum-syn)]))

(module+ test
  (require rackunit)
  (check-equal? (stx->datum (expand (stx-quote 1))) 1)
  (check-equal? (stx->datum (expand (stx-quote (let ([x 1]) x))))
                '(let ([x 1]) x))
  (check-equal? (stx->datum (expand (stx-quote (if #t 1 2))))
                '(if #t 1 2))
  (check-equal? (stx->datum (expand (stx-quote (let-syntax-rule ([(m) 2]) (m)))))
                2)
  (check-equal? (stx->datum (expand (stx-quote (let-syntax-rule ([(id x) x]) (id 2)))))
                2)
  (check-equal? (stx->datum (expand (stx-quote (let-syntax-rule ([(snd x y) y]) (snd 1 2)))))
                2)
  (check-equal? (stx->datum (expand (stx-quote (let-syntax-rule ([(or a b) (let ([tmp a]) (if tmp tmp b))]) (or 1 2)))))
                '(let ([tmp 1]) (if tmp tmp 2)))
  (check-equal? (stx->datum (expand (stx-quote (let-syntax-rule ([(or a b) (let ([tmp a]) (if tmp tmp b))])
                                                 (let-syntax-rule ([(not x) (if x #f #t)])
                                                   (let-syntax-rule ([(nor a b) (not (or a b))])
                                                     (nor 1 2)))))))
                '(if (let ([tmp 1]) (if tmp tmp 2))
                     #f
                     #t))

  ;;; stx-errors

  ;; errors will be reported as coming from here so use test-case to know where a failure came from
  (define-syntax-rule (check-stx-error program expected-who expected-message-regexp expected-expr expected-sub-expr)
    (match (expand (stx-quote program))
      [(stx-error actual-who actual-message actual-expr actual-sub-expr)
       (check-equal? actual-who 'expected-who "wrong who")
       (check-regexp-match expected-message-regexp actual-message "wrong message")
       (check-equal? (and actual-expr (stx->datum actual-expr)) 'expected-expr "wrong expr")
       (check-equal? (and actual-sub-expr (stx->datum actual-sub-expr)) 'expected-sub-expr "wrong sub-expr")]))
  (test-case "unbound var"
    (check-stx-error x x "unbound var" x #f))
  (test-case "let raw"
    (check-stx-error let let "bad syntax" let #f))
  (test-case "let misuse"
    (check-stx-error (let) let "bad syntax" (let) #f))
  (test-case "macro use extra arg"
    (check-stx-error (let-syntax-rule ([(m) 2]) (m extra))
                     m
                     "bad syntax"
                     (m extra)
                     (m extra)))
  (test-case "macro misuse specifies bad argument in sub-expr"
    (check-stx-error (let-syntax-rule ([(m 1) 2]) (m 3))
                     m
                     "bad syntax"
                     (m 3)
                     3))
  (test-case "continues after failure"
    (check-match (expand (stx-quote (if let let let)))
                 (stx-quote (if ,(? stx-error?) ,(? stx-error?) ,(? stx-error?))))
    (check-match (expand (stx-quote (let ([x unbound]) x)))
                 (stx-quote (let ([x ,(? stx-error?)]) x))))
  (test-case "malformed let lhs"
    (check-match (expand (stx-quote (let ([1 2]) 3)))
                 (stx-quote (let ([,(? stx-error?) 2]) 3))))
  (test-case "id pattern"
             (check-match (expand (stx-quote (let-syntax-rule ([m 1]) 2)))
                          (stx-quote (let-syntax-rule ([,(? stx-error?) 1]) 2))))
  (test-case "empty list pattern"
             (check-match (expand (stx-quote (let-syntax-rule ([() 1]) 2)))
                          (stx-quote (let-syntax-rule ([,(? stx-error?) 1]) 2))))
  (test-case "pattern head not identifier"
             (check-match (expand (stx-quote (let-syntax-rule ([(1) 2]) 3)))
                          (stx-quote (let-syntax-rule ([,(? stx-error?) 2]) 3)))))
;; TODO test source locations after expansion

;; symbol? stx? stx? -> (stx? -> stx?)
(define ((make-transformer who pattern template) syn)
  (expand-template template (expand-pattern who pattern syn)))

;; symbol? stx? stx? -> (or stx-error? (hash/c symbol? stx?))
;; produces mapping from pattern variable to use-site stx.
;; assumes pattern is like (var expr ...)
;; may raise a stx-error?
(define (expand-pattern who pattern syn)
  (define use-syn syn)
  (let loop ([pattern pattern]
             [syn syn])
    (cond
      [(list? (stx-e pattern))
       (define pattern-list (stx-e pattern))
       (define syn-e (stx-e syn))
       (unless (and (list? syn-e) (= (length pattern-list) (length syn-e)))
         (raise (stx-error who "bad syntax" use-syn syn)))
       (define syn-list syn-e)
       (apply hash-union
              (for/list ([p pattern-list] [s syn-list]) (loop p s))
              ;; duplicate pattern variable asserts datum equality on stx
              #:combine (lambda (a b) 
                          (if (equal? (stx->datum a) (stx->datum b))
                              a
                              (raise (stx-error who "bad syntax" use-syn b)))))]
      [(symbol? (stx-e pattern))
       (hash (stx-e pattern) syn)]
      [else
       ;; atomic
       (unless (equal? (stx->datum pattern) (stx->datum syn))
         (raise (stx-error who "bad syntax" use-syn syn)))
       (hash)])))

;; stx? (hash/c symbol? stx?) -> stx?
;; template stx with spans, env maps pattern vars to use-site stx (with their spans)
(define (expand-template template env)
  (match template
    [(stx e template-span)
     (cond
       [(symbol? e)
        ;; if it's a pattern variable, use the use-site stx (preserving its span)
        ;; otherwise, use the template stx (preserving template span)
        (hash-ref env e (lambda () template))]
       [(list? e)
        (stx (for/list ([t e])
               (expand-template t env))
             template-span)]
       [else template])]))

;; if body raises stx-error?, return it
(define-syntax-rule (with-stx-error-handling body ...)
  (with-handlers ([stx-error? (lambda (err) err)])
    body ...))