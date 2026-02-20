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

;; An Env is a
;; (hash symbol? Binding)

;; A Binding is a
(struct binding [site value] #:transparent)
;; where
;; site is a stx?, the binding occurrence
;; value is a (or #f (-> stx? stx?)), representing a value or transformer

;; An expanded? is a
(define expanded? (or/c stx? stx-error?))
;; could also be a stx? with stx-error? children

;; (mutable-hasheq stx? (listof binding?))
;; map reference site to bindings.
;; can have multiple if reference is expanded more than once.
(define bindings (make-hasheq))
;; (mutable-hasheq stx? (listof stx?))
;; map binding site to list of reference sites
(define references (make-hasheq))
;; (mutable-hasheq stx? (listof Env))
;; map stx? to the environments it was expanded under.
;; since syntax can move around and get duplicated by macros,
;; we may end up expanding it more than once.
(define environments (make-hasheq))
;; (mutable-hasheq stx? stx?)
;; map the result of macro application to its origin use-site
(define origins (make-hasheq))
;; (mutable-hasheq stx? stx?)
;; maps nodes to their surface syntax parents, sexpr-wise
(define parents (make-hasheq))

;; mutable-hash? any/c any/c -> void?
(define (hash-cons! ht key v)
  (hash-set! ht key (cons v (hash-ref ht key (list)))))

;; stx? -> (listof stx-error?)
(define (analyze! syn)
  (record-parents! syn)
  (find-stx-errors (expand syn)))

;; stx? -> void?
(define (record-parents! syn)
  (match syn
    [(stx (? list? children) _)
     (for ([child children])
       (hash-set! parents child syn)
       (record-parents! child))]
    [(stx child _)
     (hash-set! parents child syn)]))

;; stx? [stx?] [env?] -> expanded?
(define (expand syn [parent #f] [env (hasheq)])
  (hash-set! environments syn (cons env (hash-ref environments syn (list))))
  (match syn
    [(stx-quote (let ([,x ,rhs]) ,body))
     (define x-sym (stx-e x))
     (cond
       [(symbol? x-sym)
        (define rhs^ (expand rhs syn env))
        (define bnd (binding x #f))
        (hash-cons! bindings x bnd)
        (hash-cons! references x x)
        (define body^ (expand body syn (hash-set env x-sym bnd)))
        (stx-rebuild syn (let ([,x ,rhs^]) ,body^))]
       [else 
        (define err (stx-error 'let "expected a variable on the left-hand-side of binding pair" syn x))
        (define rhs^ (expand rhs syn env))
        ;; body may get unbound x, but that's ok
        (define body^ (expand body syn env))
        (stx-rebuild syn (let ([,err ,rhs^]) ,body^))])]
    [(stx-quote (if ,cnd ,thn ,els))
     (define cnd^ (expand cnd syn env))
     (define thn^ (expand thn syn env))
     (define els^ (expand els syn env))
     (stx-rebuild syn (if ,cnd^ ,thn^ ,els^))]
    [(stx-quote (let-syntax-rule ([,pattern ,template]) ,body))
     (match (stx-e pattern)
       [(cons (and m (stx (? symbol? m-sym) _)) _)
        (define transformer (make-transformer m-sym pattern template))
        (define bnd (binding m transformer))
        (hash-cons! bindings m bnd)
        (hash-cons! references m m)
        ;; TODO not sure if this origin makes sense.
        ;; let-syntax-rule is kind of like a macro that injects the body.
        (hash-set! origins body syn)
        (expand body parent (hash-set env m-sym bnd))]
       [_
        (define err (stx-error 'let-syntax-rule "pattern must be a list with an identifier head" syn pattern))
        ;; body may get unbound m, but that's ok
        ;; TODO not sure if this origin makes sense.
        ;; let-syntax-rule is kind of like a macro that injects the body.
        (hash-set! origins body syn)
        (define body^ (expand body syn env))
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
       (define bnd (hash-ref env m-sym (lambda () (raise (stx-error m-sym "unbound var" syn m)))))
       (hash-cons! bindings m bnd)
       (define binder (binding-site bnd))
       ;; this hash-ref is safe because the variable is bound, and we set the key during binding
       (hash-cons! references binder m)
       (define transformer (binding-value bnd))
       ;; no function calls, only macro calls
       (unless transformer
         (raise (stx-error m-sym "application head not a macro" syn m)))
       ;; note, stx-error from transformer is handled since we're in a with-stx-error-handling
       (define syn^ (transformer syn))
       (hash-set! origins syn^ syn)
       (expand syn^ parent env))]
    [datum-syn
     (with-stx-error-handling
       (define x-sym (stx-e datum-syn))
       (when (symbol? x-sym)
         (define bnd (hash-ref env x-sym (lambda () (raise (stx-error x-sym "unbound var" syn #f)))))
       (hash-cons! bindings datum-syn bnd)
       (define binder (binding-site bnd))
       ;; this hash-ref is safe because the variable is bound, and we set the key during binding
       (hash-cons! references binder datum-syn)
       (define transformer (binding-value bnd))
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

;; expanded? -> (listof stx-error?)
(define (find-stx-errors syn)
  (match syn
    [(? stx-error? err) (list err)]
    [(stx (? list? syns) _)
     (append-map find-stx-errors syns)]
    [_ (list)]))

;; stx? -> (set symbol?)
;; names in scope at the given syntax.
;; these names should be safe to reference.
(define (get-names-in-scope syn)
  (define environments (get-environments syn))
  (define name-sets
    (for/list ([environment environments])
      (for/seteq ([name (hash-keys environment)]) name)))
  (apply set-intersect name-sets))

(module+ test
  ;; helper for creating stx with unique ids instead of spans
  ;; exprs are numbered "left to right" with parents before their children
  (define (tagged-stx datum)
    (define count 0)
    (define (next!)
      (begin0 count
              (set! count (add1 count))))
    (let loop ([datum datum])
      (match datum
        [(? list? children)
         (define tag (next!))
         (stx (map loop children) tag)]
        [atom
         (stx atom (next!))])))
  (define (find-stx-with-tag syn tag)
    (let loop ([syn syn])
      (match syn
        [(stx _ (== tag)) syn]
        [(stx (? list? children) _)
         (for/or ([syn children])
           (loop syn))]
        [_ #f])))
  (define (find-stx-with-datum syn datum)
    (let loop ([syn syn])
      (if (equal? (stx->datum syn) datum)
          syn
          (match (stx-e syn)
            [(? list? children)
             (for/or ([syn children])
               (loop syn))]
            [_ #f]))))
  (let ([syn (tagged-stx '(let ([x 1]) 42))])
    (analyze! syn)
    (check-equal? (get-names-in-scope (find-stx-with-datum syn 42))
                  (seteq 'x)))
  (let ([syn (tagged-stx '(let ([x 1]) (let ([y 2]) 42)))])
    (analyze! syn)
    (check-equal? (get-names-in-scope (find-stx-with-datum syn 42))
                  (seteq 'x 'y))
    
    (check-equal? (get-names-in-scope (find-stx-with-datum syn 'y))
                  (seteq 'x)))
  ;; (unhygienic) macro-introduced binding is in scope, macro is in scope
  (let ([syn (tagged-stx '(let-syntax-rule ([(let-x body) (let ([x 1]) body)])
                            (let-x 2)))])
    (analyze! syn)
    (check-equal? (get-names-in-scope (find-stx-with-datum syn 2))
                  (seteq 'let-x 'x)))
  ;; (hygienic) macro-introduced binding is in scope, macro is in scope
  (let ([syn (tagged-stx '(let-syntax-rule ([(let2 x body) (let ([x 2]) body)])
                            (let2 y 3)))])
    (analyze! syn)
    (check-equal? (get-names-in-scope (find-stx-with-datum syn 3))
                  (seteq 'let2 'y)))
  ;; surface syntax gets expanded twice under different environments, unhygienically
  ;; available bindings are intersected
  (let ([syn (tagged-stx '(let-syntax-rule ([(m body) (if 1 
                                                          (let ([x 2]) (let ([y 2]) body))
                                                          (let ([y 2]) (let ([z 2]) body)))])
                            (m 3)))])
    (analyze! syn)
    (check-equal? (get-names-in-scope (find-stx-with-datum syn 3))
                  (seteq 'm 'y))))

;; stx? -> (listof Env)
;; gets environments that the expression was expanded under.
;; if none are found, tries (surface) ascendants
(define (get-environments syn)
  (cond
    [(hash-has-key? environments syn) (hash-ref environments syn)]
    [(hash-has-key? parents syn) (get-environments (hash-ref parents syn))]
    [else (list)]))

;; stx? -> (listof stx?)
(define (get-binding-sites-of syn)
  (define bnds (hash-ref bindings syn (list)))
  (for/list ([bnd bnds])
    (binding-site bnd)))

;; stx? -> (listof stx?)
(define (get-reference-sites-of syn)
  (append-map (lambda (binding-site) (hash-ref references binding-site (list)))
              (get-binding-sites-of syn)))

;; get all defined symbols from the surface syntax
(define (get-all-surface-binding-sites-in syn)
  (match (stx-e syn)
    [(? list? children)
     (append-map get-all-surface-binding-sites-in children)]
    [(? symbol?)
     (if (hash-has-key? references syn)
         (list syn)
         (list))]
    [_ (list)]))