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

  ;; run-treason : String -> String
  ;; Compiles and runs a #lang treason program, returning what it printed.
  (define (run-treason text)
    (with-output-to-string (lambda () (compile-treason text))))

  ;; error-srclocs : exn -> [Listof srcloc]
  (define (error-srclocs e) ((exn:srclocs-accessor e) e))

  (test-case
   "a well-formed program compiles and runs"
   (check-equal? (run-treason "#lang treason\n(define x 1)\nx\n") "1\n"))

  (test-case
   "an empty program compiles and prints nothing"
   (check-equal? (run-treason "#lang treason\n") ""))

  (test-case
   "a macro-using program compiles and runs"
   (check-equal?
    (run-treason
     (string-append "#lang treason\n"
                    "(define-syntax my-let\n"
                    "  (syntax-rules () [(_ ([x e]) b) (let ([x e]) b)]))\n"
                    "(define z (my-let ([q 3]) q))\n"
                    "z\n"))
    "3\n"))

  (test-case
   "every error in the file is reported, in source order, by one exception"
   (define e (compile-error "#lang treason\n(define x 1)\n(f y)\n(block (define z 2))\n"))
   (check-pred exn:fail:syntax? e)
   (check-equal? (exn-message e)
                 (string-append "treason: 3 errors\n"
                                "  3:2: f: unbound identifier\n"
                                "  3:4: y: unbound identifier\n"
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
                 "treason: 1 error\n  2:11: \"hello\": unbound identifier"))

  ;; ----------------------------------------
  ;; Running
  ;; ----------------------------------------

  (test-case
   "top-level expressions print their values; definitions print nothing"
   (check-equal? (run-treason "#lang treason\n(define x 1)\nx\n#t\n") "1\n#t\n"))

  (test-case
   "a block evaluates to its last expression"
   (check-equal? (run-treason "#lang treason\n(block (define y 2) (define z y) z)\n") "2\n"))

  (test-case
   "a block ending in a begin evaluates to the begin's last expression"
   (check-equal? (run-treason "#lang treason\n(block (begin (define y 2) y))\n") "2\n"))

  (test-case
   "an inner let shadows an outer one"
   (check-equal? (run-treason "#lang treason\n(let ([x 1]) (let ([x 2]) x))\n") "2\n"))

  (test-case
   "a definition may refer to one made later in the module"
   (check-equal? (run-treason "#lang treason\n(define a 1)\n(define b a)\nb\n") "1\n"))

  (test-case
   "a macro's binding does not capture the user's variable of the same name"
   ;; treason's hygiene has to survive into Racket: the expander tells the two
   ;; t's apart, and the compiled code must not merge them
   (check-equal?
    (run-treason
     (string-append "#lang treason\n"
                    "(define t 5)\n"
                    "(define-syntax m (syntax-rules () [(_ e) (let ([t 1]) e)]))\n"
                    "(m t)\n"))
    "5\n"))

  (test-case
   "the user's binding does not capture a macro's variable of the same name"
   (check-equal?
    (run-treason
     (string-append "#lang treason\n"
                    "(define t 5)\n"
                    "(define-syntax m (syntax-rules () [(_ e) (let ([t e]) t)]))\n"
                    "(let ([t 7]) (m t))\n"))
    "7\n"))

  (test-case
   "a variable may share its name with something Racket binds"
   ;; renamed, add becomes add1, which racket/base also binds
   (check-equal? (run-treason "#lang treason\n(define add 5)\nadd\n") "5\n"))

  (test-case
   "a variable may shadow a keyword"
   (check-equal? (run-treason "#lang treason\n(let ([let 1]) let)\n") "1\n"))

  (test-case
   "using a variable before its definition runs is a runtime error, not a compile error"
   ;; forward references are statically fine, so treason accepts this; it fails
   ;; only when the reference runs before the definition has
   (define e (compile-error "#lang treason\n(block (define a b) (define b 1) a)\n"))
   (check-pred exn:fail:contract:variable? e))

  ;; ----------------------------------------
  ;; Functions
  ;; ----------------------------------------

  (test-case
   "a lambda applied to an argument"
   (check-equal? (run-treason "#lang treason\n((lambda (x) (+ x 1)) 41)\n") "42\n"))

  (test-case
   "a recursive function"
   (check-equal?
    (run-treason
     (string-append "#lang treason\n"
                    "(define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))\n"
                    "(fact 5)\n"))
    "120\n"))

  (test-case
   "mutually recursive functions in a block"
   (check-equal?
    (run-treason
     (string-append "#lang treason\n"
                    "(block\n"
                    "  (define even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))\n"
                    "  (define odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))\n"
                    "  (even? 10))\n"))
    "#t\n"))

  (test-case
   "a closure captures its environment"
   (check-equal?
    (run-treason
     (string-append "#lang treason\n"
                    "(define make-adder (lambda (n) (lambda (x) (+ x n))))\n"
                    "((make-adder 1) 41)\n"))
    "42\n"))

  (test-case
   "a primitive is a value that can be passed around"
   (check-equal?
    (run-treason
     (string-append "#lang treason\n"
                    "(define apply2 (lambda (f a b) (f a b)))\n"
                    "(apply2 + 1 2)\n"
                    "(apply2 < 1 2)\n"
                    "(not #f)\n"))
    "3\n#t\n#t\n"))

  (test-case
   "a variable may shadow a primitive"
   (check-equal? (run-treason "#lang treason\n(let ([+ 1]) +)\n") "1\n"))

  (test-case
   "a parameter introduced by a macro does not capture the user's variable"
   (check-equal?
    (run-treason
     (string-append "#lang treason\n"
                    "(define x 5)\n"
                    "(define-syntax m (syntax-rules () [(_ e) ((lambda (x) e) 1)]))\n"
                    "(m x)\n"))
    "5\n"))

  (test-case
   "a type error in a primitive is a runtime error"
   (check-pred exn:fail:contract? (compile-error "#lang treason\n(+ 1 #t)\n")))

  (test-case
   "calling a function with the wrong number of arguments is a runtime error"
   (check-pred exn:fail:contract:arity? (compile-error "#lang treason\n((lambda (x) x))\n")))

  (test-case
   "static errors in functions are treason's, reported together"
   (define e (compile-error "#lang treason\n(lambda (x x) x)\n(if 1 2)\n()\n"))
   (check-pred exn:fail:syntax? e)
   (check-equal? (exn-message e)
                 (string-append "treason: 3 errors\n"
                                "  2:12: name already bound: x\n"
                                "  3:1: if: bad syntax\n"
                                "  4:1: empty application")))

  ;; ----------------------------------------
  ;; Runtime errors read like the program
  ;; ----------------------------------------

  (test-case
   "a runtime error names a variable as the program does, not by its renaming"
   (define e (compile-error "#lang treason\n(block (define a b) (define b 1) a)\n"))
   (check-regexp-match #rx"^b: undefined" (exn-message e)))

  (test-case
   "a function is named as the program names it"
   (check-equal? (run-treason "#lang treason\n(define f (lambda (x) x))\nf\n")
                 "#<procedure:f>\n"))

  (test-case
   "compiled code is located in the program"
   ;; Racket names an anonymous function after its source location, so this is
   ;; where the lambda sits in the treason file: line 3, column 2
   (check-regexp-match #rx":3:2>\n$"
                       (run-treason "#lang treason\n1\n  (lambda (x) x)\n")))

  (test-case
   "variables the program names alike stay distinct at runtime"
   ;; both are called t in the compiled code; hygiene has to hold regardless
   (check-equal?
    (run-treason
     (string-append "#lang treason\n"
                    "(define t 5)\n"
                    "(define-syntax m (syntax-rules () [(_ e) (let ([t 1]) (+ t e))]))\n"
                    "(m t)\n"))
    "6\n")))
