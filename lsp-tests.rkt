#lang racket

(require "server.rkt")
(module+ test (require rackunit))

;; ============================================================
;; Tests

(module+ test
  (test-case
   "basic let"
   (define source "(let ([x 2]) x)")
   (check-equal?
    (goto-definition source (find-position source "x" 1))
    (list
     (hash 'uri test-uri 'range (find-range source "x" 0))))
   (check-equal?
    (find-references source (find-position source "x" 1))
    (list
     (hash 'uri test-uri 'range (find-range source "x" 1))
     (hash 'uri test-uri 'range (find-range source "x" 0))))
    (check-equal?
     (autocomplete source (find-position source "x" 0))
     (list))
    (check-equal?
     (autocomplete source (find-position source "x" 1))
     (list (hasheq 'label "x"))))

(test-case
 "fault-tolerant"
 (define source "(let ([x unbound]) x)")
 (check-equal?
  (goto-definition source (find-position source "x" 1))
  (list
   (hash 'uri test-uri 'range (find-range source "x" 0))))
 (check-equal?
  (find-references source (find-position source "x" 1))
  (list
   (hash 'uri test-uri 'range (find-range source "x" 1))
   (hash 'uri test-uri 'range (find-range source "x" 0))))
 (check-equal?
  (autocomplete source (find-position source "x" 0))
  (list))
 (check-equal?
  (autocomplete source (find-position source "x" 1))
  (list (hasheq 'label "x"))))

(test-case
 "my-let"
 (define source
   ;; use q because x shows up in let-syntax-rule
   (~a '(let-syntax-rule ([(my-let ([q rhs]) body)
                           (let ([q rhs]) body)])
          (my-let ([q 2]) q))))
 (check-equal?
  (goto-definition source (find-position source "q" 3))
  (list
   (hash 'uri test-uri 'range (find-range source "q" 2))))
 (check-equal?
  (goto-definition source (find-position source "my-let" 1))
  (list
   (hash 'uri test-uri 'range (find-range source "my-let" 0))))
 (check-equal?
  (find-references source (find-position source "q" 3))
  (list
   (hash 'uri test-uri 'range (find-range source "q" 3))
   (hash 'uri test-uri 'range (find-range source "q" 2))))
 (check-equal?
  (find-references source (find-position source "my-let" 1))
  (list
   (hash 'uri test-uri 'range (find-range source "my-let" 1))
   (hash 'uri test-uri 'range (find-range source "my-let" 0))))
 (check-equal?
  (autocomplete source (find-position source "q" 2))
  (list (hasheq 'label "my-let")))
 (check-equal?
  (autocomplete source (find-position source "q" 3))
  (list (hasheq 'label "my-let") (hasheq 'label "q")))))

;; ============================================================
;; Test helpers

(define test-client%
  (class object%
    (super-new)
    (define/public (textDocument/publishDiagnostics . _) (void))))

(define test-uri "test.tsn")

;; Creates a server, initializes it, and opens the source as test.tsn
(define (make-test-server source)
  (define client (new test-client%))
  (define server (new server% [client client]))
  (send server initialize (hasheq))
  (send server textDocument/didOpen
        (hasheq 'textDocument (hasheq 'uri test-uri 'text source)))
  server)

;; Find the nth (0-based) occurrence of pattern in source, return LSP range
(define (find-range source pattern [index 0])
  (define lines (string-split source "\n" #:trim? #f))
  (let loop ([line-num 0] [lines lines] [count 0])
    (cond
      [(null? lines)
       (error 'find-range "pattern ~s occurrence ~a not found" pattern index)]
      [else
       (define line (car lines))
       (let inner ([start 0] [count count])
         (define pos (regexp-match-positions (regexp-quote pattern) line start))
         (cond
           [(not pos)
            (loop (add1 line-num) (cdr lines) count)]
           [(= count index)
            (define match-start (caar pos))
            (define match-end (cdar pos))
            (hash 'start (hash 'line line-num 'character match-start)
                  'end (hash 'line line-num 'character match-end))]
           [else
            (inner (cdar pos) (add1 count))]))])))

;; Find the nth occurrence of pattern, return just the start position
(define (find-position source pattern [index 0])
  (hash-ref (find-range source pattern index) 'start))

;; Self-contained goto-definition: creates server, queries, returns locations
(define (goto-definition source position)
  (define server (make-test-server source))
  (send server textDocument/definition
        (hasheq 'textDocument (hasheq 'uri test-uri)
                'position (hasheq 'line (hash-ref position 'line)
                                  'character (hash-ref position 'character)))))

;; Self-contained find-references: creates server, queries, returns locations
(define (find-references source position)
  (define server (make-test-server source))
  (send server textDocument/references
        (hasheq 'textDocument (hasheq 'uri test-uri)
                'position (hasheq 'line (hash-ref position 'line)
                                  'character (hash-ref position 'character)))))

;; Self-contained autocomplete: creates server, queries, returns completions
(define (autocomplete source position)
  (define server (make-test-server source))
  (send server textDocument/completion
        (hasheq 'textDocument (hasheq 'uri test-uri)
                'position (hasheq 'line (hash-ref position 'line)
                                  'character (hash-ref position 'character)))))