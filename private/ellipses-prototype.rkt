#lang racket

#|
prototype language with syntax rules that supports ellipses
restrictions:
- ellipses depth of pattern variables must match EXACTLY
- ellipses may only occur at the END of a list pattern (no backtracking)
|#

(module+ test (require rackunit))
(require racket/hash)

;; An Expr is one of
;; Symbol
;; Number
;; (Listof Expr)

;; A Pat is one of
;; Symbol
;; Number
;; (List Pat '...)   Ellipses only at the end of a list pattern
;; '()
;; (Cons Pat Pat)

;; A Template is one of
;; Symbol
;; Number
;; '...               Ellipses aren't restricted to only the end of a list template
;; (Listof Template)
;; A template does 

;; A Stx is a
(struct stx [e] #:transparent)
;; where e is an Expr

;; A PatternEnv is a (Hash Symbol (Rose Stx))

;; A (Rose X) is one of
;; X
;; (Listof (Rose X))

;; Pat Expr -> PatternEnv
(define (match-pattern pat expr)
  (match* (pat expr)
    [('... _)
     (error 'match-pattern "unexpected ellipsis")]
    [('_ _)
     (hash)]
    [((? symbol? x) expr)
     (hash x (stx expr))]
    [((? number? n) n)
     (hash)]
    [((? number? n) expr)
     (error 'match-pattern "expected ~a but got ~a" n expr)]
    [((list p '...) (? list? es))
     (define envs (for/list ([e es]) (match-pattern p e)))
     (combine-envs envs p)]
    [((list _ '...) expr)
     (error 'match-pattern "expected list, but got ~a" expr)]
    [((cons pa pd) (cons ea ed))
     (hash-union (match-pattern pa ea) (match-pattern pd ed))]
    [((cons _ _) expr)
     (error 'match-pattern "expected pair, but got ~a" expr)]
    [((list) (list))
    (hash)]
    [((list) expr)
     (error 'match-pattern "expected empty list, but got ~a" expr)]))

(module+ test
  (check-equal?
   (match-pattern 1 1)
   (hash))
  (check-equal?
   (match-pattern 'x 1)
   (hash 'x (stx 1)))
  (check-equal?
   (match-pattern 'x '(1 2))
   (hash 'x (stx '(1 2))))
  (check-equal?
   (match-pattern '() '())
   (hash))
  (check-equal?
   (match-pattern '(x y) '(1 2))
   (hash 'x (stx 1)
         'y (stx 2)))
  (check-equal?
   (match-pattern '(x ...) '(1))
   (hash 'x (list (stx 1))))
  (check-equal?
   (match-pattern '(x ...) '(1 2))
   (hash 'x (list (stx 1) (stx 2))))
  (check-equal?
   (match-pattern '(x ...) '())
   (hash 'x (list)))
  (check-equal?
   (match-pattern '((a b) ...) '((1 2) (3 4) (5 6)))
   (hash 'a (list (stx 1) (stx 3) (stx 5))
         'b (list (stx 2) (stx 4) (stx 6))))
  (check-equal?
   (match-pattern '(((a) b) ...) '(((1) 2) ((3) 4) ((5) 6)))
   (hash 'a (list (stx 1) (stx 3) (stx 5))
         'b (list (stx 2) (stx 4) (stx 6))))
  (check-equal?
   (match-pattern '((a ...) ...) '((1 2) (3 4 5) ()))
   (hash 'a (list (list (stx 1) (stx 2))
                  (list (stx 3) (stx 4) (stx 5))
                  (list))))
  )

;; (Listof Env) Pat -> Env
(define (combine-envs envs p)
  (define vars (pattern-free-variables p))
  (for/hash ([var vars])
    (values var (for/list ([env envs]) (hash-ref env var)))))

;; Pat -> (Set Symbol)
(define (pattern-free-variables pat)
  (match pat
    ['... (set)]
    ['_ (set)]
    [(? symbol? x) (set x)]
    [(cons a d) (set-union (pattern-free-variables a) (pattern-free-variables d))]
    [(list) (set)]))

(module+ test
  (check-equal?
   (pattern-free-variables '((x ...) _ y))
   (set 'x 'y)))

;; Template PatternEnv -> Expr
(define (expand-template tmp env)
  (match tmp
    ['... (error 'expand-template "unexpected ellipsis")]
    [(? symbol? x)
     (define expr (hash-ref env x (stx x)))
     (unless (stx? expr)
       (error 'expand-template "missing ellipsis for var ~a" x))
     ;; Note: in the real expander we won't do stx-e. we only do it here for testing convenience
     (stx-e expr)]
    [(? number? n) n]
    [(list* t '... tmp)
     (define envs (split-env env t))
     (define exprs
       (for/list ([env envs])
         (expand-template t env)))
     (append exprs (expand-template tmp env))]
    [(cons ta td)
     (cons (expand-template ta env) (expand-template td env))]
    [(list) (list)]))

;; PatternEnv Template -> (Listof Env)
(define (split-env env t)
  (define vars (template-free-variables t))
  (cond
    [(null? vars) (list)]
    [else
     (define lists
       (for/list ([var vars])
         (hash-ref env var (lambda () (error 'expand-template "unbound var ~a" var)))))
     (define lengths (for/list ([lst lists]) (length lst)))
     (unless (= (apply max lengths) (apply min lengths))
       (error 'expand-template "unequal ellipsis match counts"))
     (define len (first lengths))
     (for/list ([i (in-range len)])
       (for/hash ([var vars])
         ;; guaranteed that hash-ref will succeed since we already did this
         (define lst (hash-ref env var))
         (unless (list? lst)
           (error 'expand-template "ellipsis depth mismatch. cannot ellipsize ~a" var))
         (values var (list-ref lst i))))]))

(module+ test
  (check-equal?
   (split-env (hash 'x (list (stx 1) (stx 2))) 'x)
   (list (hash 'x (stx 1)) (hash 'x (stx 2)))))

(define template-free-variables pattern-free-variables)

(module+ test
  (check-equal?
   (expand-template 1 (hash))
   1)
  (check-equal?
   (expand-template 'x (hash 'x (stx 1)))
   1)
  (check-equal?
   (expand-template '() (hash))
   '())
  (check-equal?
   (expand-template '(x) (hash 'x (stx 1)))
   '(1))
  (check-equal?
   (expand-template '(x y) (hash 'x (stx 1)
                                 'y (stx 2)))
   '(1 2))
  (check-equal?
   (expand-template '(x ...) (hash 'x (list)))
   '())
  (check-equal?
   (expand-template '(x ...) (hash 'x (list (stx 1) (stx 2))))
   '(1 2))
  (check-equal?
   (expand-template '((x) ...) (hash 'x (list (stx 1) (stx 2))))
   '((1) (2)))
  (check-equal?
   (expand-template '(x ... y ...) (hash 'x (list (stx 1) (stx 2))
                                         'y (list (stx 3) (stx 4))))
   '(1 2 3 4))
  (check-equal?
   (expand-template '((x y) ...) (hash 'x (list (stx 1) (stx 2))
                                       'y (list (stx 3) (stx 4))))
   '((1 3) (2 4)))
  (check-equal?
   (expand-template '((a ...) ...) (hash 'a (list (list (stx 1) (stx 2))
                                                (list (stx 3) (stx 4) (stx 5))
                                                (list))))
   '((1 2) (3 4 5) ()))
  )