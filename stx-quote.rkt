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
          ;; Convert list pattern to list pattern wrapped in stx
          (let ([datum-pats (map loop (attribute datum))])
            #`(stx (list #,@datum-pats) _ _ _))]
         [atom #'(? (stx-eq-to-datum? 'atom))]))])
  (syntax-parser
    ;; TODO unquote
    [(_ stx-datum)
     #'(file-syntax->stx #'stx-datum)]))

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
      [((stx (list (stx 'm _ m-span _) (stx 'x _ _x-span _)) _ app-span _) 
        (stx (list (stx 'f _ f-span _) (stx 'y _ y-span _)) _ rebuilt-span _))
       ; literal template inherits from original
       (check-equal? f-span m-span)
       ; unquoted stx does not inherit
       (check-equal? y-span (stx-span y))
       ; list template inherits from original
       (check-equal? rebuilt-span app-span)]
      [(_ _) (fail "rebuilt syntax wrong shape")]))
  (test-case "make sure arbitrary structs can get injected"
    (struct foo [])
    (check-match (stx-rebuild (stx-quote (if 1 1 1)) (if ,(foo) ,(foo) ,(foo)))
                 (stx-quote (if ,(foo) ,(foo) ,(foo))))))

(define (stx-rebuild/proc syn e)
  (match* (syn e)
    [(_ (? (lambda (e) (or (stx? e) (not (or (pair? e) (list? e) (stx-e? e)))))))
     ;; either unquoted stx or something like a stx-error
     e]
    [((stx (? list? syn-elems) id spn marks) (? list? e-elems))
     ;; Both are proper lists - rebuild element-wise
     (stx (map stx-rebuild/proc syn-elems e-elems) id spn marks)]
    [((stx (? pair? syn-pair) id spn marks) (? pair? e-pair))
     ;; Both are cons pairs (dotted) - rebuild recursively
     (define car-rebuilt (stx-rebuild/proc (car syn-pair) (car e-pair)))
     (define cdr-rebuilt (stx-rebuild/proc (cdr syn-pair) (cdr e-pair)))
     (stx (cons car-rebuilt cdr-rebuilt) id spn marks)]
    [((stx _old-atom id spn marks) new-atom)
     (stx new-atom id spn marks)]))

;; any/c -> (stx? -> boolean?)
(define ((stx-eq-to-datum? datum) syn)
  (eq? datum (stx->datum syn)))

;; stx? -> any/c
(define (stx->datum syn)
  (define e (stx-e syn))
  (cond
    [(list? e)
     (map stx->datum e)]
    [(pair? e)
     ;; Improper list / dotted pair
     (let loop ([p e])
       (match p
         [(cons a (? pair? d)) (cons (stx->datum a) (loop d))]
         [(cons a d) (cons (stx->datum a) (stx->datum d))]))]
    [(null? e) '()]
    [else e]))

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

;; syntax? -> stx?
;; Convert a Racket syntax object to our stx representation by reading from its source file
(define (file-syntax->stx syn)
  (define src (syntax-source syn))
  (define txt (read-syntax-source-file syn))
  (define target-line (sub1 (syntax-line syn)))  ; syntax-line is 1-indexed
  (define target-col (syntax-column syn))
  ;; Find the position in the text corresponding to line/col
  (define p
    (let loop ([p 0] [l 0] [c 0])
      (cond
        [(and (= l target-line) (= c target-col)) p]
        [(>= p (string-length txt)) p]
        [(char=? (string-ref txt p) #\newline)
         (loop (add1 p) (add1 l) 0)]
        [else
         (loop (add1 p) l (add1 c))])))
  (parameterize ([current-id 0]
                 [current-pstate (pstate src txt p target-line target-col)])
    (parse)))