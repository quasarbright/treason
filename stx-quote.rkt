#lang racket

(provide
  ;; in a pattern: quasiquote for stx, with regular unquote
  ;; ex: (match syn [(stx-quote (let ([,x ,rhs]) ,body)) ...])
  ;; in an expression: quote for stx, uses racket use-site for source span.
  ;; does not support unquote
  ;; ex: (define syn (stx-quote x))
  stx-quote
  ;; a recursive quasisyntax/loc for stx.
  ;; used for updating subexpressions and maintaining source spans on unchanged pieces.
  ;; ex: (stx-rebuild syn (let ([,x ,rhs^]) ,body^))
  ;; the lists and "let" identifier will have the original source spans.
  ;; source and template must be equal? up to unquoted subexpressions.
  ;; that means lists need to be the same structure and literal symbols must be the eq?.
  stx-rebuild
  (contract-out
   [stx->datum (-> stx? any/c)]))
(module+ test (require rackunit))
(require (for-syntax syntax/parse)
         racket/file
         "reader.rkt"
         "stx.rkt")

;; in a pattern: quasisyntax with regular unquote
;; in an expression: syntax quote with no unquote
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
  (check-equal? (match (stx-quote x)
                  [(stx-quote ,x) (stx->datum x)])
                'x)
  (check-match (stx-quote (let ([x 2]) x))
               (stx-quote (let ([,_x ,_rhs]) ,_body)))
  (check-match (stx-quote (let ([x 2]) x))
               (stx-quote (let ([x 2]) x))))

;; recursive quasisyntax/loc
(define-syntax stx-rebuild
  (syntax-parser
    [(_ syn datum)
     #'(stx-rebuild/proc syn `datum)]))

(module+ test
  (check-equal? (stx->datum (stx-rebuild (stx-quote (let ([x 2]) x)) (let ([,(stx-quote y) 2]) ,(stx-quote y))))
                '(let ([y 2]) y))
  (let* ([app (stx-quote (m x))]
         [y (stx-quote y)]
         [rebuilt (stx-rebuild app (f ,y))])
    (check-equal? (stx->datum app) '(m x))
    (check-equal? (stx->datum rebuilt) '(f y))
    (match* (app rebuilt)
      [((stx (list (stx 'm m-span) (stx 'x _x-span)) app-span) 
        (stx (list (stx 'f f-span) (stx 'y y-span)) rebuilt-span))
       ; literal template inherits from original
       (check-equal? f-span m-span)
       ; unquoted stx does not inherit
       (check-equal? y-span (stx-span y))
       ; list template inherits from original
       (check-equal? rebuilt-span app-span)]
      [(_ _) (fail "rebuilt syntax wrong shape")])))

(define (stx-rebuild/proc syn e)
  (match* (syn e)
    [(_ (? stx?)) e]
    [((stx (? list? syn-es) spn) _)
     (unless (and (list? e) (= (length syn-es) (length e)))
       (error 'stx-rebuild "source and template syntax have different shape"))
     (define e^
       (for/list ([syn syn-es]
                  [e e])
         (stx-rebuild/proc syn e)))
     (stx e^ spn)]
    [((stx _old-atom spn) new-atom)
     (stx new-atom spn)]))

;; any/c -> (stx? -> boolean?)
(define ((stx-eq-to-datum? datum) syn)
  (eq? datum (stx->datum syn)))

;; stx? -> any/c
(define (stx->datum syn)
  (match (stx-e syn)
    [(? list? es) (map stx->datum es)]
    [datum datum]))

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