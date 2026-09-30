#lang racket

;; Reporting a compile of a treason module.
;;
;; The treason expander does not stop at the first error: it keeps expanding and
;; collects everything it finds. Racket's compiler reports a module's failure by
;; raising one exception, so every error found is gathered into a single one,
;; the way Typed Racket reports a module's type errors.

(provide (struct-out exn:fail:syntax:treason)
         raise-treason-errors
         collect-errors)

(require "stx.rkt"
         "expander.rkt")

;; ============================================================
;; Data Definitions
;; ============================================================

;; An exn:fail:syntax:treason carries every error found while compiling one
;; module. The message lists them all, since that is what a terminal shows;
;; the source locations are for tools that highlight them.
(struct exn:fail:syntax:treason exn:fail:syntax [errors]
  #:property prop:exn:srclocs
  (lambda (e)
    (filter-map (lambda (err)
                  (define spn (stx-error-span err))
                  (and spn (span->srcloc spn)))
                (exn:fail:syntax:treason-errors e))))

;; ============================================================
;; Reporting
;; ============================================================

;; raise-treason-errors : [Listof StxError] -> Nothing
;; Reports every error found in a module as one exception.
(define (raise-treason-errors errors)
  (raise (exn:fail:syntax:treason (errors->message errors)
                                  (current-continuation-marks)
                                  '()
                                  errors)))

;; collect-errors : ExpanderResult -> [Listof StxError]
;; Every error in an expansion, in source order.
;; The expander records errors as it finds them, and also leaves them in the
;; expanded output; the output is walked too so that an error which is only
;; embedded still gets reported. Duplicates are dropped, which works because a
;; stx-error is transparent and so compares structurally.
(define (collect-errors result)
  (sort (remove-duplicates (append (expander-result-errors result)
                                   (embedded-errors (expander-result-expanded result))))
        stx-error<?))

;; errors->message : [Listof StxError] -> String
;; One message naming every error and where it is.
(define (errors->message errors)
  (string-join (for/list ([err errors])
                 (format "  ~a~a" (error-location-prefix err) (stx-error-diagnostic-message err)))
               "\n"
               #:before-first (format "treason: ~a error~a\n"
                                      (length errors)
                                      (if (= 1 (length errors)) "" "s"))))

;; error-location-prefix : StxError -> String
;; The line and column an error should be attributed to, ready to prefix a
;; message. Empty when the error names no syntax with a location.
(define (error-location-prefix err)
  (define spn (stx-error-span err))
  (cond
    [spn
     (define start (span-start spn))
     ;; lines and columns are counted from one when shown to a person
     (format "~a:~a: " (add1 (loc-line start)) (add1 (loc-column start)))]
    [else ""]))

;; embedded-errors : XSExpr -> [Listof StxError]
;; The errors the expander left in its output.
(define (embedded-errors expanded)
  (match expanded
    [(? stx-error?) (list expanded)]
    [(? list?) (append-map embedded-errors expanded)]
    [_ '()]))

;; stx-error<? : StxError StxError -> Boolean
;; Orders errors by where they are in the source, so a report reads top to
;; bottom. Errors with no location sort last.
(define (stx-error<? a b)
  (define sa (stx-error-span a))
  (define sb (stx-error-span b))
  (cond
    [(not sa) #f]
    [(not sb) #t]
    [else
     (define la (span-start sa))
     (define lb (span-start sb))
     (or (< (loc-line la) (loc-line lb))
         (and (= (loc-line la) (loc-line lb))
              (< (loc-column la) (loc-column lb))))]))

;; ============================================================
;; Tests
;; ============================================================

(module+ test
  (require rackunit
           (only-in "reader.rkt" string->stxs))

  ;; errors-of : String -> [Listof StxError]
  (define (errors-of text)
    (collect-errors (analyze! (string->stxs "test.tsn" text))))

  (test-case
   "errors come back in source order"
   (define errors (errors-of "(f 1)\n(define x undefined-name)\n"))
   (check-equal? (map stx-error-diagnostic-message errors)
                 (list "f: unbound identifier" "undefined-name: unbound identifier")))

  (test-case
   "a clean program has no errors"
   (check-equal? (errors-of "(define x 1)\n") '()))

  (test-case
   "the message names every error and where it is"
   (define message (with-handlers ([exn:fail? exn-message])
                     (raise-treason-errors (errors-of "(f 1)\n(define x undefined-name)\n"))))
   (check-equal? message
                 (string-append "treason: 2 errors\n"
                                "  1:2: f: unbound identifier\n"
                                "  2:11: undefined-name: unbound identifier")))

  (test-case
   "the exception carries a source location for each error"
   (define e (with-handlers ([exn:fail:syntax:treason? values])
               (raise-treason-errors (errors-of "(f 1)\n(define x undefined-name)\n"))))
   (define srclocs ((exn:srclocs-accessor e) e))
   (check-equal? (map srcloc-line srclocs) (list 1 2))
   (check-equal? (map srcloc-column srclocs) (list 1 10))
   ;; line 2 starts at offset 6, so "undefined-name" is at offset 16, and it
   ;; is 14 characters wide
   (check-equal? (map srcloc-position srclocs) (list 2 17))
   (check-equal? (map srcloc-span srclocs) (list 1 14))))
