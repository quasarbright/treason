#lang racket

;; read syntax from file/string

(provide (all-defined-out))
(require "stx.rkt")

;; Read source text and compute actual line/column positions
;; path? input-port? -> stx?
(define (read-stx source in)
  ;; Read the entire source text to compute accurate end positions
  (define source-text (port->string in))
  (define syn (read-syntax source (open-input-string source-text)))
  (and syn (syntax->stx syn #f source-text)))

;; syntax? (U #f stx?) string? -> stx?
(define (syntax->stx syn parent source-text)
  (define e (syntax-e syn))
  (define src (or (syntax-source syn) 'unknown))
  (define line (syntax-line syn))
  (define col (syntax-column syn))
  (define pos (syntax-position syn))
  (define len (syntax-span syn))
  
  ;; Compute actual end line and column from source text
  (define sp
    (if (and line col pos len)
        (let* ([start-line (sub1 line)]
               [start-col col]
               [end-pos (+ pos len -1)]  ; -1 because pos is 1-indexed
               ;; Compute end line/col by counting newlines in the span
               [end-info (compute-end-position source-text (sub1 pos) end-pos)])
          (span (loc src start-line start-col)
                (loc src (car end-info) (cdr end-info))))
        ;; No source location info
        (span (loc src 0 0) (loc src 0 0))))
  
  ;; Create the stx node with placeholder element
  (define result-stx (stx 'placeholder sp parent))
  
  ;; Convert syntax-e result to StxE
  (define converted-e
    (cond
      [(list? e)
       ;; Recursively convert list elements with this stx as parent
       (for/list ([elem e])
         (syntax->stx elem result-stx source-text))]
      [else e]))  ; symbol, number, boolean, etc.
  
  ;; Use mutation to set the actual element, creating proper cyclic structure
  (set-stx-e! result-stx converted-e)
  result-stx)

;; Compute end line and column from source text
;; string? nat? nat? -> (cons nat? nat?)
(define (compute-end-position source-text start-pos end-pos)
  (define substring (substring source-text start-pos (min end-pos (string-length source-text))))
  (define lines (string-split substring "\n"))
  (define num-newlines (sub1 (length lines)))
  (define end-line num-newlines)
  (define end-col (string-length (last lines)))
  (cons end-line end-col))

(module+ test
  (require rackunit)
  
  ;; Test simple symbol
  (let* ([source-text "x"]
         [result (read-stx 'test (open-input-string source-text))])
    (check-true (stx? result))
    (check-equal? (stx-e result) 'x)
    (check-equal? (stx-parent result) #f))
  
  ;; Test simple list
  (let* ([source-text "(+ 1 2)"]
         [result (read-stx 'test (open-input-string source-text))])
    (check-true (stx? result))
    (check-true (list? (stx-e result)))
    (check-equal? (length (stx-e result)) 3)
    (check-equal? (stx-e (first (stx-e result))) '+)
    (check-equal? (stx-e (second (stx-e result))) 1)
    (check-equal? (stx-e (third (stx-e result))) 2))
  
  ;; Test parent references are cyclic
  (let* ([source-text "(foo bar)"]
         [result (read-stx 'test (open-input-string source-text))])
    (define children (stx-e result))
    (check-equal? (stx-parent (first children)) result)
    (check-equal? (stx-parent (second children)) result))
  
  ;; Test nested list with proper parent chain
  (let* ([source-text "(a (b c))"]
         [result (read-stx 'test (open-input-string source-text))])
    (define outer-children (stx-e result))
    (define inner-list (second outer-children))
    (define inner-children (stx-e inner-list))
    (check-equal? (stx-parent inner-list) result)
    (check-equal? (stx-parent (first inner-children)) inner-list)
    (check-equal? (stx-parent (second inner-children)) inner-list))
  
  ;; Test span computation for single line
  (let* ([source-text "(+ 1 2)"]
         [result (read-stx 'test (open-input-string source-text))])
    (define sp (stx-span result))
    (check-equal? (loc-line (span-start sp)) 0)
    (check-equal? (loc-column (span-start sp)) 0)
    (check-equal? (loc-line (span-end sp)) 0))
  
  ;; Test span computation for multi-line
  (let* ([source-text "(define x\n  42)"]
         [result (read-stx 'test (open-input-string source-text))])
    (define sp (stx-span result))
    (check-equal? (loc-line (span-start sp)) 0)
    (check-equal? (loc-line (span-end sp)) 1))
  
  ;; Test multiline list not starting from line 0
  (let* ([source-text "x\ny\n(foo\n bar\n baz)"]
         [result (read-stx 'test (open-input-string source-text))])
    ;; The result is the entire module-begin, get the list at line 2
    (define children (stx-e result))
    (define the-list (third children))
    (define sp (stx-span the-list))
    (check-equal? (loc-line (span-start sp)) 2)
    (check-equal? (loc-column (span-start sp)) 0)
    (check-equal? (loc-line (span-end sp)) 4)
    (check-equal? (loc-column (span-end sp)) 5))
  
  ;; Test nested multiline list with precise positions
  (let* ([source-text "(outer\n  (inner\n    a\n    b)\n  c)"]
         [result (read-stx 'test (open-input-string source-text))])
    (define outer-sp (stx-span result))
    (check-equal? (loc-line (span-start outer-sp)) 0)
    (check-equal? (loc-column (span-start outer-sp)) 0)
    (check-equal? (loc-line (span-end outer-sp)) 4)
    
    (define children (stx-e result))
    (define inner-list (second children))
    (define inner-sp (stx-span inner-list))
    (check-equal? (loc-line (span-start inner-sp)) 1)
    (check-equal? (loc-column (span-start inner-sp)) 2)
    (check-equal? (loc-line (span-end inner-sp)) 3)
    (check-equal? (loc-column (span-end inner-sp)) 6))
  
  ;; Test list starting mid-line on non-zero line
  (let* ([source-text "x\n  y  (a b c)"]
         [result (read-stx 'test (open-input-string source-text))])
    (define children (stx-e result))
    (define the-list (third children))
    (define sp (stx-span the-list))
    (check-equal? (loc-line (span-start sp)) 1)
    (check-equal? (loc-column (span-start sp)) 5)
    (check-equal? (loc-line (span-end sp)) 1))
  
  ;; Test deeply nested multiline structure
  (let* ([source-text "(a\n (b\n  (c\n   d)))"]
         [result (read-stx 'test (open-input-string source-text))])
    (define level1 (stx-e result))
    (define level2-list (second level1))
    (define level2 (stx-e level2-list))
    (define level3-list (second level2))
    (define level3 (stx-e level3-list))
    
    ;; Check spans at each level
    (check-equal? (loc-line (span-start (stx-span result))) 0)
    (check-equal? (loc-line (span-end (stx-span result))) 3)
    
    (check-equal? (loc-line (span-start (stx-span level2-list))) 1)
    (check-equal? (loc-line (span-end (stx-span level2-list))) 3)
    
    (check-equal? (loc-line (span-start (stx-span level3-list))) 2)
    (check-equal? (loc-line (span-end (stx-span level3-list))) 3))
  
  ;; Test compute-end-position helper
  (let ([text "hello\nworld"])
    (check-equal? (compute-end-position text 0 4) (cons 0 5))
    (check-equal? (compute-end-position text 0 10) (cons 1 5)))
  
  ;; Test with various data types
  (let* ([source-text "(#t #f 42 \"str\")"]
         [result (read-stx 'test (open-input-string source-text))])
    (define children (stx-e result))
    (check-equal? (stx-e (first children)) #t)
    (check-equal? (stx-e (second children)) #f)
    (check-equal? (stx-e (third children)) 42)
    (check-equal? (stx-e (fourth children)) "str"))
  
  ;; Test empty list
  (let* ([source-text "()"]
         [result (read-stx 'test (open-input-string source-text))])
    (check-true (list? (stx-e result)))
    (check-equal? (length (stx-e result)) 0)))
