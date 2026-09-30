#lang info
(define collection "treason")
(define deps '("base" "rackunit-lib" "rackcheck" "errortrace-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define compile-omit-paths '(#px"racketcon/*"))
(define test-omit-paths '(#px"racketcon/*"))
(define scribblings '(("scribblings/treason.scrbl" ())))
;; `raco pkg install` creates a `treason-language-server` executable that runs
;; server.rkt's main submodule (the LSP server over stdin/stdout).
(define racket-launcher-names '("treason-language-server"))
(define racket-launcher-libraries '("server.rkt"))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(mdelmonaco))
(define license 'MIT)
