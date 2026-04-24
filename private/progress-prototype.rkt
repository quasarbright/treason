#lang racket

(require racket/hash)

;; A PatternEnv is a (hash Symbol Expr)

;; A Progress is a (Listof ProgressStep)
;; A ProgressStep is one of
;; 'first representing the car of a list
;; 'rest representing the cdr of a list

;; try-clauses : [Listof Pattern] Expr -> (or PatternEnv (listof Pattern))
;; Tries each pattern in order until one matches. If none match, return the one(s) which failed with the most progress.
(define (try-patterns patterns expr)
  ;; (listof (cons Pattern (or PatternEnv (listof Pattern))))
  (define results
    (for/list ([pat patterns])
      (cons pat (match-top-pattern pat expr))))
  (define success
    (for/first ([result results]
                #:when (hash? (cdr result)))
      (cdr result)))
  (define failures
    (sort
     (for/list ([result results]
                #:unless (hash? (cdr result)))
       result)
     (lambda (p1 p2) (progress<? (cdr p1) (cdr p2)))))
  (cond
    [success success]
    [(not (null? failures))
     (define most-progress (cdr (last failures)))
     (for/list ([failure failures]
                #:when (equal? (cdr failure) most-progress))
       (car failure))]
    [else (list)]))

;; progress<? : Progress Progress -> Boolean
;; Returns true if p1 has made less progress than p2.
;; Longer progress means more progress (went deeper before failing).
;; For equal length, lexicographic order: 'first < 'rest.
(define (progress<? p1 p2)
  (cond
    [(< (length p1) (length p2)) #t]
    [(> (length p1) (length p2)) #f]
    [else
     ;; Equal length, lexicographic comparison
     (match* (p1 p2)
       [('() '()) #f]
       [((cons 'first _) (cons 'rest _))
        #t]
       [((cons 'rest _) (cons 'first _)) 
        #f]
       [((cons _ rest1) (cons _ rest2))
        (progress<? rest1 rest2)])]))

;; match-top-pattern : Pattern Expr -> (or PatternEnv Progress)
;; Matches a top-level pattern against syntax.
;; The car of both pattern and syntax is the macro name (ignored per syntax-rules semantics).
;; Returns a PatternEnv on success, progress on failure.
(define (match-top-pattern pat expr)
  (match* (pat expr)
    [(`(,_ . ,pd) `(,_ . ,ed))
     (with-handlers ([list? identity])
       (match-pattern pd ed (list 'rest)))]
    [(_ _) (list)]))

;; match-pattern : Pattern Expr (Id -> Bool) -> PatternEnv
;; Matches a pattern against syntax.
;; Returns a PatternEnv mapping pattern variables to matched syntax on success,
;; or raises the progress made on failure
(define (match-pattern pat expr progress-rev)
  (define (fail) (raise (reverse progress-rev)))
  (match* (pat expr)
    [((? symbol? pvar) expr)
     (hash pvar expr)]
    [(`(,pa . ,pd) `(,ea . ,ed))
     (define penv-a (match-pattern pa ea (cons 'first progress-rev)))
     (define penv-d (match-pattern pd ed (cons 'rest progress-rev)))
     (hash-union penv-a penv-d
                 #:combine (lambda (ea ed)
                             (if (equal? ea ed)
                                 ea
                                 (fail))))]
    [(_ _)
     (if  (equal? pat expr)
          (hash)
          (fail))]))

(module+ test
  (require rackunit)

  ;; try-patterns tests - verify it returns the pattern(s) with most progress on failure
  (check-equal? (try-patterns '((m 1) (m 2)) '(m 3))
                ;; Both fail at same progress, return both patterns
                '((m 1) (m 2)))
  (check-equal? (try-patterns '((m 1) (m (1 2))) '(m (1 3)))
                ;; Second pattern makes more progress, return only it
                '((m (1 2))))
  (check-equal? (try-patterns '((m (1 2)) (m 1)) '(m (1 3)))
                ;; First pattern makes more progress, return only it
                '((m (1 2))))

  ;; progress<? tests
  (check-true (progress<? '() '(first)))
  (check-true (progress<? '(first) '(first first)))
  (check-false (progress<? '(first first) '(first)))
  (check-false (progress<? '(first) '(first)))
  (check-true (progress<? '(first) '(rest)))
  (check-false (progress<? '(rest) '(first)))
  (check-true (progress<? '(first rest) '(rest first)))
  (check-false (progress<? '(rest first) '(first rest)))

  ;; match-top-pattern tests
  (check-equal? (match-top-pattern '(m p) '(m e))
                (hash 'p 'e))
  (check-equal? (match-top-pattern '(m p) '(m))
                '(rest))
  (check-equal? (match-top-pattern '(m 1) '(m 2))
                '(rest first))
  (check-equal? (match-top-pattern '(m ((1))) '(m ((2))))
                '(rest first first first))
  )