#lang racket

;; Racket syntax -> treason syntax
;;
;; The #lang treason module macro receives the whole file as Racket syntax
;; objects and hands them to the treason expander, which works on stx. This
;; module is that boundary.
;;
;; Converting loses information the compiler needs later: a treason loc records
;; only a line and column, so a Racket srcloc (which needs a character position
;; and span to highlight anything) cannot be rebuilt from a treason span. So
;; conversion also records, for every node it produces, the Racket syntax object
;; it came from, keyed by span. Reporting an error at a treason span then means
;; looking the original back up rather than reconstructing it.

(provide racket-stxs->treason
         origin-ref)

(require "stx.rkt")

;; ============================================================
;; Data Definitions
;; ============================================================

;; An OriginTable is a [MutableHashOf Span Syntax]
;; Maps the span of a converted node back to the Racket syntax object it came
;; from. Keyed by span rather than by stx because that is what the expander's
;; own tables are keyed by, and because macro expansion copies a span from a
;; template onto the syntax it introduces: the mapping is many-to-one, and only
;; ever consulted in the span -> syntax direction.

;; A LineIndex is a [Vectorof Natural]
;; Element i is the character offset at which zero-indexed line i starts.
;; Used to turn the character positions Racket records into the line/column
;; pairs a treason loc is made of.

;; ============================================================
;; Conversion
;; ============================================================

;; racket-stxs->treason : [Listof Syntax] String -> (values [Listof Stx] OriginTable [Listof StxError])
;; Converts a file's worth of Racket syntax objects into treason syntax.
;; text must be the source text the syntax objects were read from, since their
;; character positions are offsets into it.
;; Datums treason has no representation for (strings, vectors, ...) become a
;; placeholder plus an error, so that one unsupported literal does not stop the
;; rest of the file from being expanded and reported on.
(define (racket-stxs->treason rstxs text)
  (define index (make-line-index text))
  (define origins (make-hash))
  (define errors (box '()))
  (define tstxs
    (for/list ([rstx rstxs])
      (racket-stx->treason rstx index origins errors)))
  (values tstxs origins (reverse (unbox errors))))

;; origin-ref : OriginTable Span -> (or/c Syntax #f)
;; The Racket syntax object a span was converted from, if it came from one.
(define (origin-ref origins spn)
  (and spn (hash-ref origins spn #f)))

;; make-line-index : String -> LineIndex
;; Records where each line of the text starts.
(define (make-line-index text)
  (for/vector ([offset (in-sequences (in-value 0)
                                     (in-list (newline-offsets text)))])
    offset))

;; newline-offsets : String -> [Listof Natural]
;; The offset just past each newline, i.e. where each subsequent line starts.
(define (newline-offsets text)
  (for/list ([ch (in-string text)]
             [i (in-naturals)]
             #:when (char=? ch #\newline))
    (add1 i)))

;; racket-stx->treason : Syntax LineIndex OriginTable [Boxof [Listof StxError]] -> Stx
;; Converts one Racket syntax object, recording its origin and any errors.
(define (racket-stx->treason rstx index origins errors)
  (define spn (syntax->span rstx index))
  (when spn (hash-set! origins spn rstx))
  (stx (racket-stx-e->treason rstx spn index origins errors) spn '()))

;; racket-stx-e->treason : Syntax (or/c Span #f) LineIndex OriginTable [Boxof [Listof StxError]] -> StxE
;; Converts the contents of a Racket syntax object.
;; A proper list becomes a list of stx and an improper one a chain of pairs
;; ending in a stx, which is how the reader represents them.
(define (racket-stx-e->treason rstx spn index origins errors)
  (define (recur r) (racket-stx->treason r index origins errors))
  (define e (syntax-e rstx))
  (match e
    [(or (? symbol?) (? number?) (? boolean?) '()) e]
    [(? pair?)
     (let convert-pair ([e e])
       (match e
         [(? syntax?) (recur e)]
         ['() '()]
         [(cons a d) (cons (recur a) (convert-pair d))]))]
    [_ (unsupported-datum! rstx spn e errors)]))

;; unsupported-datum! : Syntax (or/c Span #f) Any [Boxof [Listof StxError]] -> StxE
;; Records an error for a datum treason cannot represent and returns a
;; placeholder to expand in its place, so expansion still reaches the rest of
;; the file. The placeholder keeps the span, so the error still points here.
(define (unsupported-datum! rstx spn datum errors)
  (define placeholder (stx 0 spn '()))
  (set-box! errors
            (cons (stx-error #f
                             (format "~a is not supported in treason" (datum-kind datum))
                             placeholder
                             #f)
                  (unbox errors)))
  0)

;; datum-kind : Any -> String
;; Names the kind of datum for an error message.
(define (datum-kind datum)
  (cond
    [(string? datum) "a string literal"]
    [(char? datum) "a character literal"]
    [(bytes? datum) "a byte string literal"]
    [(vector? datum) "a vector literal"]
    [(hash? datum) "a hash literal"]
    [(box? datum) "a box literal"]
    [(keyword? datum) "a keyword"]
    [(regexp? datum) "a regular expression literal"]
    [else (format "~s" datum)]))

;; syntax->span : Syntax LineIndex -> (or/c Span #f)
;; The source span of a Racket syntax object, or #f if it has no source
;; location. Syntax read from a file always has one.
(define (syntax->span rstx index)
  (define position (syntax-position rstx))
  (define width (syntax-span rstx))
  (and position width
       (let ([start (sub1 position)])
         (span (position->loc index (syntax-source rstx) start)
               (position->loc index (syntax-source rstx) (+ start width))))))

;; position->loc : LineIndex Any Natural -> Loc
;; Converts a zero-indexed character offset into a zero-indexed line and column.
(define (position->loc index source offset)
  (define line (offset->line index offset))
  (loc source line (- offset (vector-ref index line))))

;; offset->line : LineIndex Natural -> Natural
;; The zero-indexed line containing an offset: the last line starting at or
;; before it. Binary search, since this runs once per node per compile.
(define (offset->line index offset)
  (let search ([lo 0] [hi (sub1 (vector-length index))])
    (cond
      [(>= lo hi) lo]
      [else
       (define mid (quotient (+ lo hi 1) 2))
       (if (<= (vector-ref index mid) offset)
           (search mid hi)
           (search lo (sub1 mid)))])))

;; ============================================================
;; Tests
;; ============================================================

(module+ test
  (require rackunit
           (only-in "reader.rkt" string->stxs))

  ;; convert : String -> [Listof Stx]
  ;; Reads text with Racket's reader and converts it to treason syntax.
  (define (convert text)
    (define-values (tstxs _origins _errors) (racket-stxs->treason (read-all text) text))
    tstxs)

  ;; convert-errors : String -> [Listof String]
  (define (convert-errors text)
    (define-values (_tstxs _origins errors) (racket-stxs->treason (read-all text) text))
    (map stx-error-message errors))

  ;; read-all : String -> [Listof Syntax]
  (define (read-all text)
    (define port (open-input-string text))
    (port-count-lines! port)
    (let loop ([acc '()])
      (define s (read-syntax "test.tsn" port))
      (if (eof-object? s) (reverse acc) (loop (cons s acc)))))

  (test-case
   "conversion agrees with the treason reader, single line"
   (define text "(let ([x 2]) x)")
   (check-equal? (convert text) (string->stxs "test.tsn" text)))

  (test-case
   "conversion agrees with the treason reader, several forms over several lines"
   (define text "(define x 1)\n(block\n  (define y 2)\n  y)\n")
   (check-equal? (convert text) (string->stxs "test.tsn" text)))

  (test-case
   "conversion agrees with the treason reader on dotted pairs and brackets"
   (define text "(a . b)\n(a b . c)\n[x y]\n()")
   (check-equal? (convert text) (string->stxs "test.tsn" text)))

  (test-case
   "conversion agrees with the treason reader on atoms"
   (define text "x\n42\n#t\n#f")
   (check-equal? (convert text) (string->stxs "test.tsn" text)))

  (test-case
   "a span locates its text"
   ;; the span of the inner list covers exactly "(define y 2)"
   (define text "(block\n  (define y 2)\n  y)")
   (define blk (first (convert text)))
   (define inner (second (stx-e blk)))
   (check-equal? (stx-span inner)
                 (span (loc "test.tsn" 1 2) (loc "test.tsn" 1 14))))

  (test-case
   "every converted node is in the origin table"
   (define text "(let ([x 2]) x)")
   (define-values (tstxs origins _errors) (racket-stxs->treason (read-all text) text))
   (define (check-node t)
     (check-not-false (origin-ref origins (stx-span t))
                      (format "no origin for ~s" (stx-e t)))
     (let walk ([e (stx-e t)])
       (match e
         [(? stx?) (check-node e)]
         [(cons a d) (walk a) (walk d)]
         [_ (void)])))
   (for-each check-node tstxs))

  (test-case
   "the origin of a span is the syntax object it was converted from"
   (define text "(define x 1)"
     )
   (define-values (tstxs origins _errors) (racket-stxs->treason (read-all text) text))
   (define form (first tstxs))
   (check-equal? (syntax->datum (origin-ref origins (stx-span form))) '(define x 1))
   (check-equal? (syntax-position (origin-ref origins (stx-span form))) 1))

  (test-case
   "an unsupported datum is reported and replaced with a placeholder"
   (check-equal? (convert-errors "(define x \"hello\")")
                 (list "a string literal is not supported in treason"))
   ;; the rest of the form still converts, and the placeholder keeps the span
   (define form (first (convert "(define x \"hello\")")))
   (define placeholder (third (stx-e form)))
   (check-equal? (stx-e placeholder) 0)
   (check-equal? (stx-span placeholder)
                 (span (loc "test.tsn" 0 10) (loc "test.tsn" 0 17))))

  (test-case
   "several unsupported datums are all reported, in source order"
   (check-equal? (convert-errors "(f \"a\" #\\b '#(1))")
                 (list "a string literal is not supported in treason"
                       "a character literal is not supported in treason"
                       "a vector literal is not supported in treason")))

  (test-case
   "an unsupported datum does not stop later forms from converting"
   (define text "(define x \"a\")\n(define y 2)")
   (check-equal? (length (convert text)) 2)
   (check-equal? (length (convert-errors text)) 1)))
