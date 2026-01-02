#lang racket

;; read syntax from file/string

(provide (all-defined-out))
(require racket/syntax-srcloc "stx.rkt")

;; Read source text and compute actual line/column positions
;; path? input-port? -> (or stx? eof-object?)
(define (read-stx source in)
  ;; Read the entire source text to compute accurate end positions
  (define source-text (port->string in))
  (define text-port (open-input-string source-text source))
  (port-count-lines! text-port)
  (define syn (read-syntax source text-port))
  (if (eof-object? syn)
      syn
      (syntax->stx syn source-text)))

;; syntax? string? -> stx?
(define (syntax->stx syn source-text)
  (define e (syntax-e syn))
  (define e^
    (if (list? e)
        (for/list ([elem e])
          (syntax->stx elem source-text))
        e))

  (define sp (srcloc->span (syntax-srcloc syn) source-text))
  
  (stx e^ sp))

;; srcloc? string? -> span?
(define (srcloc->span sloc source-text)
  (match-define (srcloc src line col pos len) sloc)
  
  (unless (and line col pos len)
    (error 'srcloc->span "location data unavailable"))
  
  (define start-line (sub1 line))
  (define start-col col)
  (define-values (end-line end-col) 
    (compute-end-position source-text
                          start-line
                          start-col
                          (sub1 pos); sub1 because pos is 1-indexed
                          (sub1 (+ pos len))))
  (span (loc (or src 'unknown) start-line start-col)
        (loc (or src 'unknown) end-line end-col)))

;; Compute end line and column from source text
;; all arguments are zero-indexed
;; enx-pos is exclusive
;; string? nat? nat? nat? nat? -> (values nat? nat?)
(define (compute-end-position source-text start-line start-col start-pos end-pos)
  (define substr (substring source-text start-pos end-pos))
  (define lines (string-split substr "\n"))
  (define num-newlines (sub1 (length lines)))
  (define end-line (+ start-line num-newlines))
  (define end-col
    (if (zero? num-newlines)
        (+ start-col (string-length (last lines)))
        (string-length (last lines))))
  (values end-line end-col))

(module+ test
  (require rackunit)
  
  ;; Test simple symbol
  (check-equal?
   (read-stx 'test (open-input-string "x"))
   (stx 'x (span (loc 'test 0 0) (loc 'test 0 1))))
  
  ;; Test simple list
  (check-equal?
   (read-stx 'test (open-input-string "(+ 1 2)"))
   (stx (list (stx '+ (span (loc 'test 0 1) (loc 'test 0 2)))
              (stx 1 (span (loc 'test 0 3) (loc 'test 0 4)))
              (stx 2 (span (loc 'test 0 5) (loc 'test 0 6))))
        (span (loc 'test 0 0) (loc 'test 0 7))))
  
  ;; Test nested list
  (check-equal?
   (read-stx 'test (open-input-string "(a (b c))"))
   (stx (list (stx 'a (span (loc 'test 0 1) (loc 'test 0 2)))
              (stx (list (stx 'b (span (loc 'test 0 4) (loc 'test 0 5)))
                         (stx 'c (span (loc 'test 0 6) (loc 'test 0 7))))
                   (span (loc 'test 0 3) (loc 'test 0 8))))
        (span (loc 'test 0 0) (loc 'test 0 9))))
  
  ;; Test multiline list not starting from line 0
  (check-equal?
   (read-stx 'test (open-input-string "(x\ny\n(foo\n bar\n baz))"))
   (stx (list (stx 'x (span (loc 'test 0 1) (loc 'test 0 2)))
              (stx 'y (span (loc 'test 1 0) (loc 'test 1 1)))
              (stx (list (stx 'foo (span (loc 'test 2 1) (loc 'test 2 4)))
                         (stx 'bar (span (loc 'test 3 1) (loc 'test 3 4)))
                         (stx 'baz (span (loc 'test 4 1) (loc 'test 4 4))))
                   (span (loc 'test 2 0) (loc 'test 4 5))))
        (span (loc 'test 0 0) (loc 'test 4 6))))
  
  ;; Test nested multiline list with precise positions
  (check-equal?
   (read-stx 'test (open-input-string "(outer\n  (inner\n    a\n    b)\n  c)"))
   (stx (list (stx 'outer (span (loc 'test 0 1) (loc 'test 0 6)))
              (stx (list (stx 'inner (span (loc 'test 1 3) (loc 'test 1 8)))
                         (stx 'a (span (loc 'test 2 4) (loc 'test 2 5)))
                         (stx 'b (span (loc 'test 3 4) (loc 'test 3 5))))
                   (span (loc 'test 1 2) (loc 'test 3 6)))
              (stx 'c (span (loc 'test 4 2) (loc 'test 4 3))))
        (span (loc 'test 0 0) (loc 'test 4 4))))
  
  ;; Test list starting mid-line on non-zero line
  (check-equal?
   (read-stx 'test (open-input-string "(x\n  y  (a b c))"))
   (stx (list (stx 'x (span (loc 'test 0 1) (loc 'test 0 2)))
              (stx 'y (span (loc 'test 1 2) (loc 'test 1 3)))
              (stx (list (stx 'a (span (loc 'test 1 6) (loc 'test 1 7)))
                         (stx 'b (span (loc 'test 1 8) (loc 'test 1 9)))
                         (stx 'c (span (loc 'test 1 10) (loc 'test 1 11))))
                   (span (loc 'test 1 5) (loc 'test 1 12))))
        (span (loc 'test 0 0) (loc 'test 1 13))))
  
  ;; Test deeply nested multiline structure
  (check-equal?
   (read-stx 'test (open-input-string "(a\n (b\n  (c\n   d)))"))
   (stx (list (stx 'a (span (loc 'test 0 1) (loc 'test 0 2)))
              (stx (list (stx 'b (span (loc 'test 1 2) (loc 'test 1 3)))
                         (stx (list (stx 'c (span (loc 'test 2 3) (loc 'test 2 4)))
                                    (stx 'd (span (loc 'test 3 3) (loc 'test 3 4))))
                              (span (loc 'test 2 2) (loc 'test 3 5))))
                   (span (loc 'test 1 1) (loc 'test 3 6))))
        (span (loc 'test 0 0) (loc 'test 3 7))))
  
  ;; Test with various data types
  (check-equal?
   (read-stx 'test (open-input-string "(#t #f 42 \"str\")"))
   (stx (list (stx #t (span (loc 'test 0 1) (loc 'test 0 3)))
              (stx #f (span (loc 'test 0 4) (loc 'test 0 6)))
              (stx 42 (span (loc 'test 0 7) (loc 'test 0 9)))
              (stx "str" (span (loc 'test 0 10) (loc 'test 0 15))))
        (span (loc 'test 0 0) (loc 'test 0 16))))
  
  ;; Test empty list
  (check-equal?
   (read-stx 'test (open-input-string "()"))
   (stx '() (span (loc 'test 0 0) (loc 'test 0 2))))

  ;; Test eof
  (check-equal?
   (read-stx 'test (open-input-string ""))
   eof)

  
  ;; Test compute-end-position helper
  (let ([text "hello\nworld"])
    ;; substring from 0 to 4 is "hell", so end is at column 4 on line 0
    (check-equal? (compute-end-position text 0 0 0 4) (cons 0 4))
    ;; substring from 0 to 10 is "hello\nworl", so end is at column 4 on line 1
    (check-equal? (compute-end-position text 0 0 0 10) (cons 1 4))))
