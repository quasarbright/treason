#lang racket/base

;; The #lang treason language.
;;
;; The reader hands the whole module body across as one form holding the
;; program's syntax, so the expander sees the file at once, which is what
;; analyze! expects and what lets it report every error in the file rather than
;; stopping at the first. A program with no errors is compiled to Racket; its
;; top-level expressions print their values, as in #lang racket.

(provide (rename-out [module-begin #%module-begin]))

(require (for-syntax racket/base
                     ;; only these: the expander provides all of its
                     ;; definitions, some of which shadow racket/base
                     (only-in "../expander.rkt"
                              analyze! expander-result-expanded expander-result-state
                              expander-state-renamings expander-state-origins)
                     (only-in "read.rkt" parse-treason-text)
                     "../diagnostics.rkt"
                     "../codegen.rkt"))

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ (_tag src text))
     (let* ([stxs (parse-treason-text (syntax->datum #'src) (syntax->datum #'text))]
            [result (analyze! stxs)]
            [errors (collect-errors result)])
       (unless (null? errors)
         (raise-treason-errors errors))
       (define state (expander-result-state result))
       #`(#%printing-module-begin
          #,@(xsexpr->module-body (expander-result-expanded result) stx
                                  #:renamings (expander-state-renamings state)
                                  #:origins (expander-state-origins state))))]))
