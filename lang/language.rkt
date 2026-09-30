#lang racket/base

;; The #lang treason language.
;;
;; The reader hands the whole module body across as one form holding the
;; program's syntax, so the expander sees the file at once, which is what
;; analyze! expects and what lets it report every error in the file rather than
;; stopping at the first.

(provide (rename-out [module-begin #%module-begin]))

(require (for-syntax racket/base
                     ;; only analyze!: the expander provides all of its
                     ;; definitions, some of which shadow racket/base
                     (only-in "../expander.rkt" analyze!)
                     (only-in "read.rkt" parse-treason-text)
                     "../diagnostics.rkt"))

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ (_tag src text))
     (let* ([stxs (parse-treason-text (syntax->datum #'src) (syntax->datum #'text))]
            [errors (collect-errors (analyze! stxs))])
       (unless (null? errors)
         (raise-treason-errors errors))
       ;; Compiling the expanded program to Racket is not implemented yet, so a
       ;; module that expands cleanly runs as an empty module.
       #'(#%plain-module-begin))]))
