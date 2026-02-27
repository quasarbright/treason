#lang racket

;; Property-based tests for the reader.
;; Core property: parsing a string produces stx nodes whose spans
;; correspond to the correct substrings of the original source.

(require "reader.rkt" "stx.rkt")
(require rackcheck rackunit)

;; ============================================================
;; Generators
;; ============================================================

;; gen:char-lower : Gen Char
;; Generates a lowercase ASCII letter.
(define gen:char-lower
  (gen:let ([c (gen:char-in (char->integer #\a) (char->integer #\z))])
    c))

;; gen:symbol-string : Gen String
;; Generates a valid symbol name (lowercase letters, 1-6 chars).
(define gen:symbol-string
  (gen:let ([chars (gen:list gen:char-lower #:max-length 6)])
    (define s (list->string chars))
    (if (string=? s "") "a" s)))

;; gen:number-string : Gen String
;; Generates a string representation of a small natural number.
(define gen:number-string
  (gen:let ([n (gen:integer-in 0 999)])
    (number->string n)))

;; gen:boolean-string : Gen String
;; Generates "#t" or "#f".
(define gen:boolean-string
  (gen:choice (gen:const "#t") (gen:const "#f")))

;; gen:atom-string : Gen String
;; Generates a string that is a valid atom (symbol, number, or boolean).
(define gen:atom-string
  (gen:choice gen:symbol-string gen:number-string gen:boolean-string))

;; gen:sexp-string-of : Natural -> Gen String
;; Generates a string that is a valid s-expression, bounded by depth.
(define (gen:sexp-string-of depth)
  (if (<= depth 0)
      gen:atom-string
      (gen:choice
       gen:atom-string
       (gen:let ([elems (gen:list (gen:sexp-string-of (sub1 depth))
                                  #:max-length 4)])
         (string-append "(" (string-join elems " ") ")")))))

;; gen:sexp-string : Gen String
;; Top-level generator for s-expression strings.
(define gen:sexp-string
  (gen:sexp-string-of 3))

;; ============================================================
;; Helpers
;; ============================================================

;; offset-at : String Natural Natural -> Natural
;; Computes the character offset in text for a given (line, col) pair.
(define (offset-at text target-line target-col)
  (define lines (string-split text "\n" #:trim? #f))
  (+ (for/sum ([l (in-list lines)]
               [i (in-naturals)]
               #:break (= i target-line))
       (+ (string-length l) 1))
     target-col))

;; span-substring : String Span -> String
;; Extracts the substring of text corresponding to a span.
(define (span-substring text sp)
  (define start-offset (offset-at text
                                   (loc-line (span-start sp))
                                   (loc-column (span-start sp))))
  (define end-offset (offset-at text
                                 (loc-line (span-end sp))
                                 (loc-column (span-end sp))))
  (substring text start-offset end-offset))

;; stx-datum->string : (or/c Symbol Number Boolean) -> String
;; Converts an atom datum back to its string representation.
(define (stx-datum->string v)
  (cond
    [(symbol? v) (symbol->string v)]
    [(number? v) (number->string v)]
    [(boolean? v) (if v "#t" "#f")]))

;; collect-atoms : Stx -> (Listof Stx)
;; Collects all atom (leaf) stx nodes from a syntax tree.
(define (collect-atoms syn)
  (match syn
    [(stx (? list? elems) _ _ _)
     (append-map collect-atoms elems)]
    [(stx (? pair? p) _ _ _)
     (append (collect-atoms (car p)) (collect-atoms (cdr p)))]
    [(stx '() _ _ _) '()]
    [(stx _ _ _ _)
     (list syn)]))

;; collect-lists : Stx -> (Listof Stx)
;; Collects all list-level stx nodes that have a span.
(define (collect-lists syn)
  (match syn
    [(stx (? list? elems) _ (? span?) _)
     (cons syn (append-map collect-lists elems))]
    [(stx (? list? elems) _ _ _)
     (append-map collect-lists elems)]
    [(stx (? pair? p) _ (? span?) _)
     (cons syn (append (collect-lists (car p)) (collect-lists (cdr p))))]
    [(stx (? pair? p) _ _ _)
     (append (collect-lists (car p)) (collect-lists (cdr p)))]
    [_ '()]))

;; ============================================================
;; Tests
;; ============================================================

(module+ test

  ;; Property: every atom's span extracts the correct datum string
  ;; from the source.
  (test-case
   "property: atom spans correspond to source substrings"
   (check-property
    (property ([src gen:sexp-string])
      (define parsed (string->stx 'test src))
      (for ([atom (in-list (collect-atoms parsed))])
        (define sp (stx-span atom))
        (when sp
          (define extracted (span-substring src sp))
          (define expected (stx-datum->string (stx-e atom)))
          (check-equal? extracted expected
                        (format "atom ~v: span extracted ~v from ~v"
                                (stx-e atom) extracted src)))))))

  ;; Property: list node spans start with ( and end with ).
  (test-case
   "property: list spans are delimited by parens"
   (check-property
    (property ([src gen:sexp-string])
      (define parsed (string->stx 'test src))
      (for ([lst (in-list (collect-lists parsed))])
        (define sp (stx-span lst))
        (when sp
          (define extracted (span-substring src sp))
          (check-true (char=? (string-ref extracted 0) #\()
                      (format "list span should start with ( in ~v" src))
          (check-true (char=? (string-ref extracted (sub1 (string-length extracted))) #\))
                      (format "list span should end with ) in ~v" src)))))))

  ;; Property: all atom spans are non-empty.
  (test-case
   "property: atom spans are non-empty"
   (check-property
    (property ([src gen:sexp-string])
      (define parsed (string->stx 'test src))
      (for ([atom (in-list (collect-atoms parsed))])
        (define sp (stx-span atom))
        (when sp
          (define start (offset-at src (loc-line (span-start sp))
                                       (loc-column (span-start sp))))
          (define end (offset-at src (loc-line (span-end sp))
                                     (loc-column (span-end sp))))
          (check-true (< start end)
                      (format "span should be non-empty for ~v in ~v"
                              (stx-e atom) src))))))))
