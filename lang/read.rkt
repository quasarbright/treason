#lang racket

;; Reading the body of a #lang treason module.
;;
;; The body is read by treason's own reader, not Racket's. A #lang treason file
;; and the language server therefore accept exactly the same surface syntax, and
;; a malformed file fails with treason's parse error rather than Racket's read
;; error, which is what lets the compiler report it in treason's terms.
;;
;; The reader is handed a port positioned after the #lang line, but every span
;; has to name a location in the whole file. Rather than teach the reader to
;; start somewhere other than the beginning, the text it reads is padded with
;; whitespace standing in for the #lang line, which lands every span where it
;; belongs.

(provide padded-source-text
         parse-treason-text
         read-treason-body
         source-prefix)

(require "../reader.rkt"
         "../stx.rkt")

;; read-treason-body : Any InputPort -> [Listof Stx]
;; Reads a whole module body as treason syntax, with spans that locate the file
;; rather than the body. Raises exn:fail:read for a malformed file.
(define (read-treason-body src in)
  (parse-treason-text src (padded-source-text in)))

;; padded-source-text : InputPort -> String
;; The rest of the port, padded so that it sits where it does in the file.
;; This text, rather than the syntax read from it, is what the reader hands to
;; the compiler: a struct type is generated afresh for each instantiation of the
;; module defining it, so syntax built by the reader is not of the same type as
;; the syntax the expander works with one phase up. Text crosses phases; the
;; compiler parses it again on the other side.
(define (padded-source-text in)
  (define-values (line column position) (port-next-location in))
  (string-append (source-prefix (or line 1) (or column 0) (or position 1))
                 (port->string in)))

;; parse-treason-text : Any String -> [Listof Stx]
;; Parses padded module text, reporting a malformed file as a read error.
(define (parse-treason-text src text)
  (with-handlers ([exn:fail:parse? raise-as-read-error])
    (string->stxs src text)))

;; source-prefix : Natural Natural Natural -> String
;; Whitespace standing in for the text before the module body, given where the
;; body starts: its line and position counted from one, its column from zero.
;; The prefix is as long as the text it replaces and holds as many newlines, and
;; ends with as many spaces as the body's column, so that reading the padded
;; text gives every span the line, column and offset it has in the file.
(define (source-prefix line column position)
  (define newlines (sub1 line))
  (define leading (max 0 (- position 1 newlines column)))
  (string-append (make-string leading #\space)
                 (make-string newlines #\newline)
                 (make-string column #\space)))

;; raise-as-read-error : exn:fail:parse -> Nothing
;; Reports a treason parse error as a Racket read error, so that tools which
;; expect one still learn where in the file the problem is.
;; The location goes in the message as well as the source location, since that
;; is where Racket's own read errors put it and all a terminal shows.
(define (raise-as-read-error e)
  (define spn (exn:fail:parse-span e))
  (define srcloc (and spn (span->srcloc spn)))
  (raise (exn:fail:read (if srcloc
                            (format "~a:~a:~a: ~a"
                                    (srcloc-source srcloc)
                                    (srcloc-line srcloc)
                                    (srcloc-column srcloc)
                                    (exn-message e))
                            (exn-message e))
                        (exn-continuation-marks e)
                        (if srcloc (list srcloc) '()))))

;; ============================================================
;; Tests
;; ============================================================

(module+ test
  (require rackunit)

  ;; read-body : String -> [Listof Stx]
  ;; Reads text as a module body, the way the #lang reader is handed one: from a
  ;; port already positioned past the #lang line.
  (define (read-body text)
    (define port (open-input-string text))
    (port-count-lines! port)
    (read-line port)
    (read-treason-body "prog.tsn" port))

  (test-case
   "the prefix stands in for the text before the body"
   ;; "#lang treason" is 13 characters, so the body starts at line 1, column 13,
   ;; position 14: the prefix is 13 spaces and no newline
   (check-equal? (source-prefix 1 13 14) (make-string 13 #\space))
   ;; a body starting on the third line keeps the newlines before it
   (check-equal? (source-prefix 3 2 12) "       \n\n  ")
   ;; and the prefix is always as long as the text it replaces
   (check-equal? (string-length (source-prefix 3 2 12)) 11))

  (test-case
   "spans locate the file, not the body"
   (define stxs (read-body "#lang treason\n(define x 1)\n(f x)\n"))
   (check-equal? (length stxs) 2)
   ;; (define x 1) is on line 1, column 0, at offset 14
   (check-equal? (stx-span (first stxs))
                 (span (loc "prog.tsn" 1 0 14) (loc "prog.tsn" 1 12 26))))

  (test-case
   "a span still slices its own text out of the file"
   (define text "#lang treason\n(define x 1)\n(f x)\n")
   (define stxs (read-body text))
   (for ([s stxs] [expected '("(define x 1)" "(f x)")])
     (define sl (span->srcloc (stx-span s)))
     (check-equal? (substring text (sub1 (srcloc-position sl))
                              (+ (sub1 (srcloc-position sl)) (srcloc-span sl)))
                   expected)))

  (test-case
   "an empty body reads as no forms"
   (check-equal? (read-body "#lang treason\n") '()))

  (test-case
   "a malformed body is a read error that locates the file"
   (define e (with-handlers ([exn:fail:read? values])
               (read-body "#lang treason\n(define x 1\n")))
   (check-pred exn:fail:read? e)
   (check-equal? (exn-message e) "prog.tsn:2:0: unexpected end of input in list")
   (define sl (first (exn:fail:read-srclocs e)))
   (check-equal? (srcloc-line sl) 2)
   (check-equal? (srcloc-column sl) 0)
   (check-equal? (srcloc-position sl) 15)))
