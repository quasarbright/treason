#lang racket

;; concrete syntax tree

(provide (all-defined-out))

;; A Surface_Node_ID is a Natural.
;; Unique identifier for surface syntax nodes, assigned during parsing.
;; Macro-introduced nodes have #f instead.

;; A Mark is a Symbol created by gensym.
;; Marks distinguish identifiers introduced at different macro expansion sites.

;; A Stx is a
(struct stx [e id span marks] #:transparent)
;; where
;; e is a StxE
;; id is a (or/c Natural #f) - Surface_Node_ID, #f for macro-introduced
;; span is a Span (or #f)
;; marks is a (Listof Mark) - hygiene marks (most recent first)
;; Represents concrete syntax of a program

;; A StxE is one of
;; symbol
;; number
;; boolean
;; '()
;; (cons Stx StxE) - proper list, e.g. (a b c) -> (list stx stx stx)
;; (cons Stx Stx) - improper list, e.g. (a b . c) -> (cons stx (cons stx stx))
(define stx-e? (flat-rec-contract stx-e-ctc
                 (or/c symbol? number? boolean? null?
                       (cons/c stx? stx-e-ctc)
                       (cons/c stx? stx?))))

;; An Identifier is a Syntax where (stx-e stx) is a Symbol.
;; Predicates and accessors:
(define (identifier? x) (and (stx? x) (symbol? (stx-e x))))
(define (identifier-symbol id) (stx-e id))
(define (identifier-marks id) (stx-marks id))
(define (identifier-id id) (stx-id id))
(define (identifier-span id) (stx-span id))

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

;; ============================================================
;; Stx Accessors
;; ============================================================

;; stx-car : Stx -> Stx
;; Gets the first element of a stx list or pair.
(define (stx-car syn)
  (match syn
    [(stx (cons a _) _ _ _) a]
    [(stx (list a _ ...) _ _ _) a]))

;; stx-cdr : Stx -> Stx or (Listof Stx)
;; Gets the rest of a stx list or pair.
;; For a proper list, returns a stx wrapping the rest of the list.
;; For a dotted pair, returns the cdr stx.
(define (stx-cdr syn)
  (match syn
    [(stx (cons _ d) _ _ _) d]
    [(stx (list _ rest ...) id spn marks) (stx rest id spn marks)]))

;; stx-list : Stx -> (Listof Stx)
;; Converts a stx list to a Racket list.
(define (stx-list syn)
  (match syn
    [(stx (? list? elems) _ _ _) elems]))

;; ============================================================
;; Errors
;; ============================================================

;; A StxError represents a syntax error during expansion.
(struct stx-error [who message stx sub-stx] #:transparent)
;; who : (or/c Symbol #f) - the form that detected the error (e.g., 'let)
;; message : String - error description
;; stx : Stx - the syntax where the error occurred
;; sub-stx : (or/c Stx #f) - more specific location within stx, if any
