#lang racket

(provide (all-defined-out))
(module+ test (require rackunit))
(require (for-syntax syntax/parse)
         racket/file
         "reader.rkt"
         "stx.rkt")

;; syntax? -> string?
;; return the contents of the file that syn came from
(define (read-syntax-source-file syn)
  (define source (syntax-source syn))
  (cond
    [(path? source) (file->string source)]
    [(path-string? source) (file->string source)]
    [else (error 'read-syntax-source-file 
                  "cannot read source file from syntax object: ~a" 
                  source)]))

;; a simple quasiquote for stx. uses regular unquote for convenience. also supports stx-unquote
(define-match-expander stx-quote
  (syntax-parser
    [(_ datum)
     (let loop ([datum #'datum])
       (syntax-parse datum
         #:datum-literals (stx-unquote unquote)
         [(stx-unquote pat) #'pat]
         [(unquote pat) #'pat]
         [(datum ...)
          #`(stx (list #,@(map loop (attribute datum))) _)]
         [atom #'(? (stx-eq-to-datum? 'atom))]))])
  (syntax-parser
    ;; TODO unquote
    [(_ stx)
     #'(syntax->stx #'stx (read-syntax-source-file #'stx))]))

(module+ test
  (check-equal? (stx->datum (stx-quote x))
                'x)
  (check-equal? (match (stx-quote x)
                  [(stx-quote ,x) (stx->datum x)])
                'x)
  (check-equal? (match (stx-quote (let ([x 2]) x))
                  [(stx-quote (let ([,(stx x x-span) ,rhs]) ,body)) 2])
                2))

;; any/c -> (stx? -> boolean?)
(define ((stx-eq-to-datum? datum) syn)
  (eq? datum (stx->datum syn)))

;; stx? -> any/c
(define (stx->datum syn)
  (match (stx-e syn)
    [(? list? es) (map stx->datum es)]
    [datum datum]))
