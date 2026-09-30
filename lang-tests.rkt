#lang racket

;; Tests for #lang treason, driven the way a user meets it: by writing a file
;; and asking Racket to load it.
;;
;; The exception a failed compile raises is caught here as exn:fail:syntax with
;; source locations, not as the more specific treason exception. A struct type
;; is generated afresh for each instantiation of the module defining it, and the
;; exception is raised by the compiler one phase up from this one, so its
;; specific predicate would not recognise it here. Anything catching a treason
;; compile error from outside is in the same position, which is why the
;; exception is a subtype of exn:fail:syntax and carries source locations.

(module+ test
  (require rackunit)

  ;; compile-treason : String -> Any
  ;; Writes a #lang treason file and loads it, so that the whole path runs:
  ;; module name resolution, treason's reader, and the treason expander.
  (define (compile-treason text)
    (define path (make-temporary-file "treason~a.rkt"))
    (dynamic-wind
     void
     (lambda ()
       (display-to-file text path #:exists 'truncate)
       (dynamic-require `(file ,(path->string path)) #f))
     (lambda () (delete-file path))))

  ;; compile-error : String -> exn
  (define (compile-error text)
    (with-handlers ([(lambda (_) #t) values]) (compile-treason text)))

  ;; error-srclocs : exn -> [Listof srcloc]
  (define (error-srclocs e) ((exn:srclocs-accessor e) e))

  (test-case
   "a well-formed program compiles"
   (check-not-exn (lambda () (compile-treason "#lang treason\n(define x 1)\nx\n"))))

  (test-case
   "an empty program compiles"
   (check-not-exn (lambda () (compile-treason "#lang treason\n"))))

  (test-case
   "a macro-using program compiles"
   (check-not-exn
    (lambda ()
      (compile-treason
       (string-append "#lang treason\n"
                      "(define-syntax my-let\n"
                      "  (syntax-rules () [(_ ([x e]) b) (let ([x e]) b)]))\n"
                      "(define z (my-let ([q 3]) q))\n"
                      "z\n")))))

  (test-case
   "every error in the file is reported, in source order, by one exception"
   (define e (compile-error "#lang treason\n(define x 1)\n(f y)\n(block (define z 2))\n"))
   (check-pred exn:fail:syntax? e)
   (check-equal? (exn-message e)
                 (string-append "treason: 2 errors\n"
                                "  3:2: f: unbound identifier\n"
                                "  4:8: block: block must end in an expression")))

  (test-case
   "an error's source location is where it is in the file, #lang line included"
   (define text "#lang treason\n(define x 1)\n(f y)\n")
   (define e (compile-error text))
   (define sl (first (error-srclocs e)))
   ;; line 3 of the file, and the offset slices the "f" out of "(f y)"
   (check-equal? (srcloc-line sl) 3)
   (check-equal? (srcloc-column sl) 1)
   (check-equal? (substring text (sub1 (srcloc-position sl))
                            (+ (sub1 (srcloc-position sl)) (srcloc-span sl)))
                 "f"))

  (test-case
   "a malformed file fails as a read error that locates the file"
   (define e (compile-error "#lang treason\n(define x 1\n"))
   (check-pred exn:fail:read? e)
   (define sl (first (exn:fail:read-srclocs e)))
   (check-equal? (srcloc-line sl) 2)
   (check-equal? (srcloc-column sl) 0))

  (test-case
   "treason's reader is the one in charge, not Racket's"
   ;; Racket would read "hello" as a string. treason's reader has no strings, so
   ;; the quotes are just characters in a symbol, which is then unbound. The
   ;; point is that Racket's reader never sees the file; that a quote can end up
   ;; inside a symbol is a separate wart in the reader.
   (define e (compile-error "#lang treason\n(define x \"hello\")\n"))
   (check-pred exn:fail:syntax? e)
   (check-equal? (exn-message e)
                 "treason: 1 error\n  2:11: \"hello\": unbound identifier")))
