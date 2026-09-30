#lang s-exp syntax/module-reader
treason/lang/language
#:whole-body-readers? #t
#:read (lambda (in) (list (program-form #f in)))
#:read-syntax (lambda (src in) (list (datum->syntax #f (program-form src in))))

;; The whole module body is handed across as a single form, so that the expander
;; sees the file at once. What is handed over is the body's text, not the syntax
;; read from it, since the two sides are different instantiations of the modules
;; defining that syntax; the compiler parses the text again on its side. Parsing
;; here as well is what makes a malformed file fail as a read error, at read
;; time, the way a #lang is expected to fail.

(require "read.rkt")

;; program-form : Any InputPort -> (list Symbol Any String)
(define (program-form src in)
  (define text (padded-source-text in))
  (parse-treason-text src text)
  (list '#%treason-program src text))
