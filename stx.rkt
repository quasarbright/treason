#lang racket

;; concrete syntax tree

(provide (all-defined-out))

;; A Stx is a
(struct stx [e span] #:transparent)
;; where
;; e is a StxE
;; span is a Span
;; Represents concrete syntax of a program
;; TODO add fields for macro expansion, like origin/previous

;; A StxE is one of
;; symbol
;; number
;; boolean
;; (Listof Stx)

;; A Span is a
(struct span [start end] #:transparent)
;; where
;; start and end are Locations
;; end is exclusive
;; Represents a range of source code text

;; A Loc is a
(struct loc [source line column] #:transparent)
;; where
;; source identifies the source, often a file path
;; line is a natural representing line number, zero-indexed
;; col is a natural representing the offset in that line, zero-indexed
;; Represents a location in a program source
