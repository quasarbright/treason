#lang racket

;; read syntax from file/string
;; Custom s-expression parser that produces stx structures with
;; cons-based pairs where both car and cdr are wrapped in stx.

(provide (all-defined-out))
(require "stx.rkt")

;; ============================================================
;; Parse Error Type
;; ============================================================

;; An exn:fail:parse is a parse error with a source span.
;; span : (or/c Span #f) — location in source where the error occurred
(struct exn:fail:parse exn:fail [span] #:transparent)

;; Parser state: source, text, and mutable position tracking
(struct pstate [source text pos line col] #:mutable #:transparent)

;; parameter? pstate?
;; Current parser state, mutated during parsing.
(define current-pstate (make-parameter #f))

;; string? (or/c Span #f) -> (raises exn:fail:parse)
;; Raises a parse error with the given message and source span.
(define (parse-error! msg sp)
  (raise (exn:fail:parse msg (current-continuation-marks) sp)))

;; any/c string? -> stx?
;; Parses text as an s-expression and returns a stx structure.
;; src is used for location tracking (typically a file path or symbol).
(define (string->stx src txt)
  (parameterize ([current-pstate (pstate src txt 0 0 0)])
    (parse)))

;; any/c string? -> [Listof Stx]
;; Parses text as a sequence of s-expressions, returning all top-level forms.
(define (string->stxs src txt)
  (parameterize ([current-pstate (pstate src txt 0 0 0)])
    (let loop ([acc '()])
      (define s (parse))
      (if (eof-object? s)
          (reverse acc)
          (loop (cons s acc))))))

;; -> (or/c stx? eof)
;; Parses an s-expression from text at the current position.
;; Returns stx or eof.
(define (parse)
  (skip-whitespace!)
  (cond
    [(at-end?) eof]
    [else
     (define ch (current-char))
     (cond
       [(or (char=? ch #\() (char=? ch #\[))
        (parse-list)]
       [(char=? ch #\#)
        (parse-hash)]
       [(char=? ch #\')
        (parse-quote)]
       [else
        (parse-atom)])]))

;; -> any/c
;; Returns the source identifier.
(define (source) (pstate-source (current-pstate)))

;; -> string?
;; Returns the source text.
(define (text) (pstate-text (current-pstate)))

;; -> natural?
;; Returns the current position.
(define (pos) (pstate-pos (current-pstate)))

;; -> natural?
;; Returns the current line.
(define (line) (pstate-line (current-pstate)))

;; -> natural?
;; Returns the current column.
(define (col) (pstate-col (current-pstate)))

;; natural? natural? natural? -> void?
;; Sets the position part of parser state.
(define (set-pos! p l c)
  (define ps (current-pstate))
  (set-pstate-pos! ps p)
  (set-pstate-line! ps l)
  (set-pstate-col! ps c))

;; -> void?
;; Advances position by one character, updating line/col appropriately.
(define (advance!)
  (define p (pos))
  (define t (text))
  (when (< p (string-length t))
    (if (char=? (string-ref t p) #\newline)
        (set-pos! (add1 p) (add1 (line)) 0)
        (set-pos! (add1 p) (line) (add1 (col))))))

;; -> char?
;; Returns the current character, or #f if at end.
(define (current-char)
  (if (< (pos) (string-length (text)))
      (string-ref (text) (pos))
      #f))

;; -> boolean?
;; Returns #t if at end of input.
(define (at-end?)
  (>= (pos) (string-length (text))))

;; -> void?
;; Skips whitespace and comments, mutating the parser state.
(define (skip-whitespace!)
  (let loop ()
    (unless (at-end?)
      (define ch (current-char))
      (cond
        [(char=? ch #\newline)
         (advance!)
         (loop)]
        [(char-whitespace? ch)
         (advance!)
         (loop)]
        [(char=? ch #\;)
         ;; Skip to end of line
         (advance!)
         (let skip-comment ()
           (unless (at-end?)
             (define c (current-char))
             (cond
               [(char=? c #\newline)
                (advance!)
                (loop)]
               [else
                (advance!)
                (skip-comment)])))]))))

;; -> stx?
;; Parses a list: ( ... ) or [ ... ] or ( ... . tail ).
;; Handles both parentheses and brackets as delimiters.
(define (parse-list)
  (define start-line (line))
  (define start-col (col))
  (define open-char (current-char))
  (define close-char (if (char=? open-char #\() #\) #\]))
  
  ;; Skip opening paren/bracket
  (advance!)
  
  (let loop ([elements '()])
    (skip-whitespace!)
    (cond
      [(at-end?)
       (parse-error! "unexpected end of input in list"
                     (span (loc (source) start-line start-col)
                           (loc (source) (line) (col))))]
      [(char=? (current-char) close-char)
       ;; End of list
       (advance!)
       (define sp (span (loc (source) start-line start-col)
                        (loc (source) (line) (col))))
       (define e (reverse elements))
       (stx e sp '())]
      [(and (or (char=? (current-char) #\))
                (char=? (current-char) #\]))
            (not (char=? (current-char) close-char)))
       ;; Mismatched closing delimiter — raise error to avoid infinite loop
       (parse-error! (format "unexpected ~a, expected ~a" (current-char) close-char)
                     (span (loc (source) (line) (col))
                           (loc (source) (line) (add1 (col)))))]
      [(char=? (current-char) #\.)
       ;; Check if this is a dot for improper list
       (define next-pos (add1 (pos)))
       (if (and (< next-pos (string-length (text)))
                (let ([next-ch (string-ref (text) next-pos)])
                  (or (char-whitespace? next-ch)
                      (char=? next-ch #\()
                      (char=? next-ch #\[)
                      (char=? next-ch #\))
                      (char=? next-ch #\]))))
           ;; It's a dot - parse the tail
           (let ()
             (when (null? elements)
               (parse-error! "unexpected dot at beginning of list"
                             (span (loc (source) (line) (col))
                                   (loc (source) (line) (add1 (col))))))
             (advance!)  ; skip the dot
             (skip-whitespace!)
             (define tail-stx (parse))
             (skip-whitespace!)
             (unless (and (not (at-end?))
                          (char=? (current-char) close-char))
               (parse-error! (format "expected ~a after dotted tail" close-char)
                             (span (loc (source) start-line start-col)
                                   (loc (source) (line) (col)))))
             (advance!)  ; skip closing paren
             (define sp (span (loc (source) start-line start-col)
                              (loc (source) (line) (col))))
             (define e (build-improper-list-stx (reverse elements) tail-stx))
             (stx e sp '()))
           ;; It's an atom starting with dot
           (loop (cons (parse-atom) elements)))]
      [else
       ;; Parse next element
       (loop (cons (parse) elements))])))


;; (listof stx?) stx? -> stx-e?
;; Builds an improper list (dotted pair) stx-e.
;; elements is the list of car elements, tail-stx is the final cdr.
;; Returns a cons chain: (cons stx (cons stx ... tail-stx)).
(define (build-improper-list-stx elements tail-stx)
  (if (null? elements)
      (stx-e tail-stx)  ; shouldn't happen for well-formed input
      (let loop ([elems elements])
        (if (null? (cdr elems))
            (cons (car elems) tail-stx)
            (cons (car elems) (loop (cdr elems)))))))

;; -> stx?
;; Parses an atom (symbol or number).
(define (parse-atom)
  (define start-line (line))
  (define start-col (col))
  (define start-pos (pos))

  ;; Find end of atom
  (let loop ()
    (unless (at-end?)
      (define ch (current-char))
      (unless (or (char-whitespace? ch)
                  (char=? ch #\()
                  (char=? ch #\))
                  (char=? ch #\[)
                  (char=? ch #\])
                  (char=? ch #\;))
        (advance!)
        (loop))))

  (define atom-str (substring (text) start-pos (pos)))
  (when (string=? atom-str "")
    (parse-error! "unexpected delimiter, expected atom"
                  (span (loc (source) start-line start-col)
                        (loc (source) (line) (col)))))
  (define atom-val
    (or (string->number atom-str)
        (string->symbol atom-str)))
  (define sp (span (loc (source) start-line start-col)
                   (loc (source) (line) (col))))
  (stx atom-val sp '()))

;; -> stx?
;; Parses #t, #f, or other # forms.
(define (parse-hash)
  (define start-line (line))
  (define start-col (col))
  
  (cond
    [(>= (add1 (pos)) (string-length (text)))
     (parse-error! "unexpected end of input after #"
                   (span (loc (source) start-line start-col)
                         (loc (source) (line) (col))))]
    [else
     (define next-ch (string-ref (text) (add1 (pos))))
     (cond
       [(char=? next-ch #\t)
        (advance!)  ; skip #
        (advance!)  ; skip t
        (define sp (span (loc (source) start-line start-col)
                         (loc (source) (line) (col))))
        (stx #t sp '())]
       [(char=? next-ch #\f)
        (advance!)  ; skip #
        (advance!)  ; skip f
        (define sp (span (loc (source) start-line start-col)
                         (loc (source) (line) (col))))
        (stx #f sp '())]
       [(char=? next-ch #\%)
        ;; Parse #%identifier (e.g., #%expression)
        (parse-atom)]
       [else
        (parse-error! (format "unsupported # form: #~a" next-ch)
                      (span (loc (source) start-line start-col)
                            (loc (source) (line) (col))))])]))
;; -> stx?
;; Parses 'expr as (quote expr).
(define (parse-quote)
  (define start-line (line))
  (define start-col (col))
  
  ;; Skip the quote character
  (advance!)
  
  (define inner (parse))
  (define sp (span (loc (source) start-line start-col)
                   (loc (source) (line) (col))))
  (define quote-sym (stx 'quote
                         (span (loc (source) start-line start-col)
                               (loc (source) start-line (add1 start-col)))
                         '()))
  (define e (list quote-sym inner))
  (stx e sp '()))

;; ============================================================
;; S-Expression Conversion
;; ============================================================

;; sexpr->syntax : SExpression -> Stx
;; Converts a plain s-expression to syntax.
;; All nodes get #f span (macro-introduced, no source location).
;; Proper lists become (stx (list ...)), dotted pairs become (stx (cons ...)).
(define (sexpr->syntax s)
  (match s
    [(? list? elems)
     (stx (map sexpr->syntax elems) #f '())]
    [(cons a d)
     ;; Improper list / dotted pair
     (stx (let loop ([s s])
            (match s
              [(cons a (? pair? d)) (cons (sexpr->syntax a) (loop d))]
              [(cons a d) (cons (sexpr->syntax a) (sexpr->syntax d))]))
          #f '())]
    [(? symbol? s) (stx s #f '())]
    [(? number? n) (stx n #f '())]
    [_ s]))

;; syntax->sexpr : Stx -> SExpression
;; Converts syntax back to a plain s-expression.
(define (syntax->sexpr syn)
  (match syn
    [(stx (? list? elems) _ _)
     (map syntax->sexpr elems)]
    [(stx (cons a d) _ _)
     ;; Improper list / dotted pair
     (let loop ([e (stx-e syn)])
       (match e
         [(cons a (? pair? d)) (cons (syntax->sexpr a) (loop d))]
         [(cons a d) (cons (syntax->sexpr a) (syntax->sexpr d))]))]
    [(stx e _ _)
     (cond
       [(symbol? e) e]
       [else e])]
    [(cons a d) (cons (syntax->sexpr a) (syntax->sexpr d))]
    [(? symbol? s) s]
    [(? number? n) n]
    ['() '()]
    [_ syn]))

(module+ test
  (require rackunit)
  
  ;; Helper to check stx structure ignoring IDs
  (define-syntax-rule (check-stx-equal? actual expected-pattern)
    (check-match actual expected-pattern))
  
  ;; Test simple symbol
  (check-stx-equal?
   (string->stx 'test "x")
   (stx 'x (span (loc 'test 0 0) (loc 'test 0 1)) '()))
  
  ;; Test simple list (as flat Racket list of stx elements)
  (check-stx-equal?
   (string->stx 'test "(+ 1 2)")
   (stx (list (stx '+ (span (loc 'test 0 1) (loc 'test 0 2)) '())
              (stx 1 (span (loc 'test 0 3) (loc 'test 0 4)) '())
              (stx 2 (span (loc 'test 0 5) (loc 'test 0 6)) '()))
        (span (loc 'test 0 0) (loc 'test 0 7)) '()))
  
  ;; Test dotted pair (improper list)
  (check-stx-equal?
   (string->stx "test.tsn" "(foo . bar)")
   (stx (cons (stx 'foo (span (loc "test.tsn" 0 1) (loc "test.tsn" 0 4)) '())
              (stx 'bar (span (loc "test.tsn" 0 7) (loc "test.tsn" 0 10)) '()))
        (span (loc "test.tsn" 0 0) (loc "test.tsn" 0 11)) '()))
  
  ;; Test empty list
  (check-stx-equal?
   (string->stx 'test "()")
   (stx '() (span (loc 'test 0 0) (loc 'test 0 2)) '()))

  ;; Test eof
  (check-equal?
   (parameterize ([current-pstate (pstate 'test "" 0 0 0)])
     (parse))
   eof)
  
  ;; Test nested list
  (check-stx-equal?
   (string->stx 'test "(a (b c))")
   (stx (list (stx 'a (span (loc 'test 0 1) (loc 'test 0 2)) '())
              (stx (list (stx 'b (span (loc 'test 0 4) (loc 'test 0 5)) '())
                         (stx 'c (span (loc 'test 0 6) (loc 'test 0 7)) '()))
                   (span (loc 'test 0 3) (loc 'test 0 8)) '()))
        (span (loc 'test 0 0) (loc 'test 0 9)) '()))
  
  ;; Test booleans
  (check-stx-equal?
   (string->stx 'test "(#t #f)")
   (stx (list (stx #t (span (loc 'test 0 1) (loc 'test 0 3)) '())
              (stx #f (span (loc 'test 0 4) (loc 'test 0 6)) '()))
        (span (loc 'test 0 0) (loc 'test 0 7)) '()))
  
  ;; Test multiline
  (check-stx-equal?
   (string->stx 'test "(a\nb)")
   (stx (list (stx 'a (span (loc 'test 0 1) (loc 'test 0 2)) '())
              (stx 'b (span (loc 'test 1 0) (loc 'test 1 1)) '()))
        (span (loc 'test 0 0) (loc 'test 1 2)) '()))
  
  ;; Test comments
  (check-stx-equal?
   (string->stx 'test "(a ; comment\nb)")
   (stx (list (stx 'a (span (loc 'test 0 1) (loc 'test 0 2)) '())
              (stx 'b (span (loc 'test 1 0) (loc 'test 1 1)) '()))
        (span (loc 'test 0 0) (loc 'test 1 2)) '()))

  ;; ============================================================
  ;; Quote parsing
  ;; ============================================================

  ;; Test quote of symbol: 'x -> (quote x)
  (let ([syn (string->stx 'test "'x")])
    ;; Outer stx wraps a two-element list: (quote x)
    (check-match syn (stx (list (stx 'quote _ _) (stx 'x _ _)) _ _))
    ;; Span covers the whole 'x form
    (check-equal? (stx-span syn)
                  (span (loc 'test 0 0) (loc 'test 0 2)))
    ;; Quote symbol span covers just the ' character
    (check-equal? (stx-span (car (stx-e syn)))
                  (span (loc 'test 0 0) (loc 'test 0 1)))
    ;; Inner symbol span covers just x
    (check-equal? (stx-span (cadr (stx-e syn)))
                  (span (loc 'test 0 1) (loc 'test 0 2))))

  ;; Test quote of list: '(a b) -> (quote (a b))
  (let ([syn (string->stx 'test "'(a b)")])
    (check-match syn (stx (list (stx 'quote _ _)
                                (stx (list (stx 'a _ _) (stx 'b _ _)) _ _))
                      _ _))
    ;; Outer span covers '(a b)
    (check-equal? (stx-span syn)
                  (span (loc 'test 0 0) (loc 'test 0 6))))

  ;; Test quote of number: '42 -> (quote 42)
  (check-match (string->stx 'test "'42")
               (stx (list (stx 'quote _ _) (stx 42 _ _)) _ _))

  ;; ============================================================
  ;; Bracket lists
  ;; ============================================================

  ;; Test bracket list: [a b] parsed same as (a b)
  (check-stx-equal?
   (string->stx 'test "[a b]")
   (stx (list (stx 'a (span (loc 'test 0 1) (loc 'test 0 2)) '())
              (stx 'b (span (loc 'test 0 3) (loc 'test 0 4)) '()))
        (span (loc 'test 0 0) (loc 'test 0 5)) '()))

  ;; Test empty bracket list
  (check-stx-equal?
   (string->stx 'test "[]")
   (stx '() (span (loc 'test 0 0) (loc 'test 0 2)) '()))

  ;; ============================================================
  ;; Improper lists (more than one element before dot)
  ;; ============================================================

  ;; Test improper list: (a b . c)
  (check-stx-equal?
   (string->stx 'test "(a b . c)")
   (stx (cons (stx 'a (span (loc 'test 0 1) (loc 'test 0 2)) '())
              (cons (stx 'b (span (loc 'test 0 3) (loc 'test 0 4)) '())
                    (stx 'c (span (loc 'test 0 7) (loc 'test 0 8)) '())))
        (span (loc 'test 0 0) (loc 'test 0 9)) '()))

  ;; ============================================================
  ;; Hash forms
  ;; ============================================================

  ;; Test standalone #t
  (check-stx-equal?
   (string->stx 'test "#t")
   (stx #t (span (loc 'test 0 0) (loc 'test 0 2)) '()))

  ;; Test standalone #f
  (check-stx-equal?
   (string->stx 'test "#f")
   (stx #f (span (loc 'test 0 0) (loc 'test 0 2)) '()))

  ;; Test #%identifier
  (check-stx-equal?
   (string->stx 'test "#%expression")
   (stx '#%expression (span (loc 'test 0 0) (loc 'test 0 12)) '()))

  ;; ============================================================
  ;; Atom edge cases
  ;; ============================================================

  ;; Test standalone number
  (check-stx-equal?
   (string->stx 'test "42")
   (stx 42 (span (loc 'test 0 0) (loc 'test 0 2)) '()))

  ;; Test negative number
  (check-stx-equal?
   (string->stx 'test "-7")
   (stx -7 (span (loc 'test 0 0) (loc 'test 0 2)) '()))

  ;; Test atom starting with dot (not a dotted pair separator)
  (check-match (string->stx 'test "(.foo)")
               (stx (list (stx '.foo _ _)) _ _))

  ;; Test mismatched closing delimiter raises parse error (not infinite loop)
  (check-exn exn:fail:parse?
             (lambda () (string->stx 'test "([x)]")))

  ;; ============================================================
  ;; sexpr->syntax / syntax->sexpr round-trip
  ;; ============================================================

  ;; Proper list round-trip
  (check-equal?
   (syntax->sexpr (sexpr->syntax '(a b c)))
   '(a b c))

  ;; Dotted pair round-trip
  (check-equal?
   (syntax->sexpr (sexpr->syntax '(a . b)))
   '(a . b))

  ;; Improper list round-trip
  (check-equal?
   (syntax->sexpr (sexpr->syntax '(a b . c)))
   '(a b . c))

  ;; Nested list round-trip
  (check-equal?
   (syntax->sexpr (sexpr->syntax '(let ([x 1]) x)))
   '(let ([x 1]) x))

  ;; Empty list round-trip
  (check-equal?
   (syntax->sexpr (sexpr->syntax '()))
   '())

  ;; Atom round-trips
  (check-equal?
   (syntax->sexpr (sexpr->syntax 'foo))
   'foo)
  (check-equal?
   (syntax->sexpr (sexpr->syntax 42))
   42))
