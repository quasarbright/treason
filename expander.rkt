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

(provide (all-defined-out))
(require "stx.rkt"
         "stx-quote.rkt"
         racket/hash)

;; data definitions

;; a Transformer is a
;; (stx? -> stx?)
;; which represents a stx transformer from a let-syntax-rule macro definition

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
                     #t)))
;; TODO test source locations after expansion

;; stx? [(hash symbol? (or/c #f (stx? -> stx?)))] -> stx?
;; env maps symbols to transformers or #f (for normal var)
(define (expand syn [env (hash)])
  (match syn
    [(stx-quote (,(and let-stx (stx-quote let)) ,(and binding (stx-quote ([,x ,rhs]))) ,body))
     (define x-sym (stx->datum x))
     (unless (symbol? x-sym)
       (error 'expand "let binding must be a symbol, got ~a" x-sym))
     (define expanded-rhs (expand rhs env))
     (define expanded-body (expand body (hash-set env x-sym #f)))
     (stx (list (stx 'let (stx-span let-stx))
                (stx (list (stx (list x expanded-rhs) (stx-span binding))) ; technically should be inner parens but whatever
                     (stx-span binding))
                expanded-body)
          (stx-span syn))]
    [(stx-quote (,(and if-stx (stx-quote if)) ,cnd ,thn ,els))
     (stx (list (stx 'if (stx-span if-stx))
                (expand cnd env)
                (expand thn env)
                (expand els env))
          (stx-span syn))]
    [(stx-quote (let-syntax-rule ([,pattern ,template]) ,body))
     (unless (match (stx-e pattern) [(list _ ...) #t] [_ #f])
       (error 'expand "invalid macro pattern: pattern must be a list"))
     (match (stx-e pattern)
       [(cons m _)
        (define m-sym (stx->datum m))
        (unless (symbol? m-sym)
          (error 'expand "macro name must be a symbol"))
        (define transformer (make-transformer pattern template))
        (expand body (hash-set env m-sym transformer))]
       [_ (error 'expand "invalid macro pattern")])]
    [(stx (cons m _) _)
     (define m-sym (stx->datum m))
     (unless (symbol? m-sym)
       (error 'expand "application head must be a symbol"))
     (define transformer (hash-ref env m-sym (lambda () (error 'expand "unbound var: ~a" m-sym))))
     ;; no function calls, only macro calls
     (unless transformer
       (error 'expand "applied non-macro: ~a" m-sym))
     (define syn^ (transformer syn))
     (expand syn^ env)]
    [datum-syn
     (define x-sym (stx->datum datum-syn))
     (when (symbol? x-sym)
       (define transformer (hash-ref env x-sym (lambda () (error 'expand "unbound var: ~a" x-sym))))
       (when transformer
         (error x-sym "bad syntax")))
     datum-syn]))

;; stx? stx? -> (stx? -> stx?)
(define ((make-transformer pattern template) syn)
  (expand-template template (expand-pattern pattern syn)))

;; stx? stx? -> (hash/c symbol? stx?)
;; produces mapping from pattern variable to use-site stx.
;; assumes pattern is like (var expr ...)
(define (expand-pattern pattern syn)
  (define pattern-e (stx-e pattern))
  (unless (and (list? pattern-e) (not (null? pattern-e)))
    (error 'expand-pattern "pattern must be a non-empty list"))
  (define m (car pattern-e))
  (define m-sym (stx->datum m))
  (let loop ([pattern pattern]
             [syn syn])
    (cond
      [(list? (stx-e pattern))
       (define pattern-list (stx-e pattern))
       (define syn-e (stx-e syn))
       (unless (and (list? syn-e) (= (length pattern-list) (length syn-e)))
         (error m-sym "bad syntax"))
       (define syn-list syn-e)
       (apply hash-union 
              (for/list ([p pattern-list] [s syn-list]) (loop p s))
              #:combine/key (lambda (k a b) (error m-sym "duplicate pattern variable: ~a" k)))]
      [(symbol? (stx-e pattern))
       (hash (stx-e pattern) syn)]
      [else
       ;; atomic
       (unless (equal? (stx->datum pattern) (stx->datum syn))
         (error m-sym "bad syntax"))
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