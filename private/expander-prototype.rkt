#lang racket

;; simple prototype of a non hygienic expander

(require racket/hash)

;; expr [(hash symbol (or #f (expr -> expr)))] -> expr
;; env maps symbols to transformers or #f (for normal var)
(define (expand expr [env (hash)])
  (match expr
    [`(let ([,(? symbol? x) ,rhs]) ,body)
     `(let ([,x ,(expand rhs env)]) ,(expand body (hash-set env x #f)))]
    [`(if ,cnd ,thn ,els)
     `(if ,(expand cnd env)
          ,(expand thn env)
          ,(expand els env))]
    [`(let-syntax-rule ([,(and pattern `(,m . ,_)) ,template]) ,body)
     (define transformer (make-transformer pattern template))
     (expand body (hash-set env m transformer))]
    [`(,m . ,_)
     (define transformer (hash-ref env m (lambda () (error 'expand "unbound var: ~a" m))))
     ;; no function calls, only macro calls
     (unless transformer
       (error 'expand "applied non-macro: ~a" m))
     (define expr^ (transformer expr))
     (expand expr^ env)]
    [(? symbol? x)
     (define transformer (hash-ref env x (lambda () (error 'expand "unbound var: ~a" x))))
     (when transformer
       (error x "bad syntax"))
     x]
    [datum datum]))

(module+ test
  (require rackunit)
  (check-equal? (expand 1) 1)
  (check-equal? (expand '(let ([x 1]) x))
                '(let ([x 1]) x))
  (check-equal? (expand '(if #t 1 2))
                '(if #t 1 2))
  (check-equal? (expand '(let-syntax-rule ([(m) 2]) (m)))
                2)
  (check-equal? (expand '(let-syntax-rule ([(id x) x]) (id 2)))
                2)
  (check-equal? (expand '(let-syntax-rule ([(snd x y) y]) (snd 1 2)))
                2)
  (check-equal? (expand '(let-syntax-rule ([(or a b) (let ([tmp a]) (if tmp tmp b))]) (or 1 2)))
                '(let ([tmp 1]) (if tmp tmp 2)))
  (check-equal? (expand '(let-syntax-rule ([(or a b) (let ([tmp a]) (if tmp tmp b))])
                           (let-syntax-rule ([(not x) (if x #f #t)])
                             (let-syntax-rule ([(nor a b) (not (or a b))])
                               (nor 1 2)))))
                '(if (let ([tmp 1]) (if tmp tmp 2))
                     #f
                     #t)))

;; expr expr -> (expr -> expr)
(define ((make-transformer pattern template) expr)
  (expand-template template (expand-pattern pattern expr)))

;; expr expr -> (hash symbol expr)
;; produces mapping from pattern variable to use-site expr
(define (expand-pattern pattern expr)
  (match pattern
    [`(,(? symbol? m) . ,_)
     (let loop ([pattern pattern]
                [expr expr])
       (cond
         [(list? pattern)
          (unless (and (list? expr) (= (length pattern) (length expr)))
            (error m "bad syntax"))
          (apply hash-union 
                 (for/list ([pattern pattern] [expr expr]) (loop pattern expr))
                 #:combine/key (lambda (k a b) (error m "duplicate pattern variable: ~a" k)))]
         [(symbol? pattern) (hash pattern expr)]
         [else
          ;; atomic
          (unless (equal? pattern expr)
            (error m "bad syntax"))
          (hash)]))]))

;; expr (hash symbol expr) -> expr
(define (expand-template template env)
  (match template
    [(? symbol? x)
     (hash-ref env x (lambda () x))]
    [(? list? templates)
     (for/list ([template templates])
       (expand-template template env))]
    [atom atom]))