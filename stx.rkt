#lang racket

;; concrete syntax tree

(provide (all-defined-out))

;; A Mark is a Symbol created by gensym.
;; Marks distinguish identifiers introduced at different macro expansion sites.

;; A Stx is a
(struct stx [e span marks] #:transparent)
;; where
;; e is a StxE
;; span is a Span (or #f)
;; marks is a (Listof Mark) - hygiene marks (most recent first)
;; Represents concrete syntax of a program

;; A StxE is one of
;; symbol
;; number
;; boolean
;; '()
;; (cons Stx StxE) - proper list, e.g. (a b c) -> (list stx stx stx)
;; (cons Stx Stx) - improper list, e.g. (a b . c) -> (cons stx (cons stx stx))
(define stx-e? (flat-rec-contract stx-e-ctc
                 (or/c symbol? number? boolean? null?
                       (cons/c stx? stx-e-ctc)
                       (cons/c stx? stx?))))

;; An Identifier is a Syntax where (stx-e stx) is a Symbol.
;; Predicates and accessors:
(define (identifier? x) (and (stx? x) (symbol? (stx-e x))))
(define (identifier-symbol id) (stx-e id))
(define (identifier-marks id) (stx-marks id))
(define (identifier-span id) (stx-span id))

;; A Span is a
(struct span [start end] #:transparent)
;; where
;; start and end are Locations
;; end is exclusive
;; Represents a range of source code text

;; A Loc is a
(struct loc [source line column position] #:transparent)
;; where
;; source identifies the source, often a file path
;; line is a natural representing line number, zero-indexed
;; col is a natural representing the offset in that line, zero-indexed
;; position is a (or/c Natural #f): the offset from the start of the source,
;;   zero-indexed, counted in characters
;; Represents a location in a program source
;;
;; Locations from the reader always have a position. It is #f only for a
;; location that did not come from reading a source, such as an editor cursor,
;; which arrives as a line and column with no offset. Line and column are what
;; locations are ordered and compared by; position exists because a Racket
;; srcloc needs an offset and a width, which line and column cannot supply.

;; span->srcloc : Span -> srcloc?
;; Converts a span to a Racket source location, so that code compiled from
;; treason can report errors against treason source. Racket counts lines from
;; one and positions from one, and measures a span as a width in characters.
;; The position and width are #f for a span whose locations carry no position.
(define (span->srcloc spn)
  (define start (span-start spn))
  (define position (loc-position start))
  (define end-position (loc-position (span-end spn)))
  (srcloc (loc-source start)
          (add1 (loc-line start))
          (loc-column start)
          (and position (add1 position))
          (and position end-position (- end-position position))))

;; ============================================================
;; Stx Accessors
;; ============================================================

;; stx-car : Stx -> Stx
;; Gets the first element of a stx list or pair.
(define (stx-car syn)
  (match syn
    [(stx (cons a _) _ _) a]
    [(cons a _) a]))

;; stx-cdr : Stx -> Stx or (Listof Stx)
;; Gets the rest of a stx list or pair.
;; For a proper list, returns a stx wrapping the rest of the list.
;; For a dotted pair, returns the cdr stx.
(define (stx-cdr syn)
  (match syn
    [(stx (cons _ d) _ _) d]
    [(cons _ d) d]))

;; ============================================================
;; Errors
;; ============================================================

;; A StxError represents a syntax error during expansion.
(struct stx-error [who message stx sub-stx] #:transparent)
;; who : (or/c Symbol #f) - the form that detected the error (e.g., 'let)
;; message : String - error description
;; stx : Stx - the syntax where the error occurred
;; sub-stx : (or/c Stx #f) - more specific location within stx, if any

;; ============================================================
;; Tests
;; ============================================================

(module+ test
  (require rackunit)

  (test-case
   "a span becomes a Racket source location"
   ;; "(f x)\n(g)" — the (g) form starts at offset 6 and is 3 characters wide
   (define spn (span (loc "p.tsn" 1 0 6) (loc "p.tsn" 1 3 9)))
   (check-equal? (span->srcloc spn) (srcloc "p.tsn" 2 0 7 3)))

  (test-case
   "a span whose locations carry no position still locates a line and column"
   (define spn (span (loc "p.tsn" 1 0 #f) (loc "p.tsn" 1 3 #f)))
   (check-equal? (span->srcloc spn) (srcloc "p.tsn" 2 0 #f #f))))
