#lang racket

(require racket/hash)

;; A PatternEnv is a (hash Symbol Expr)

;; A Progress is a (Listof ProgressStep)
;; A ProgressStep is one of
;; 'first representing the car of a list
;; 'rest representing the cdr of a list

;; A MatchFailure is a (match-failure Progress (Listof Expr))
;; progress : Progress - how far matching got before failing
;; ose-exprs : (Listof Expr) - subexpressions from ~var to optimistically expand
(struct match-failure [progress ose-exprs] #:transparent)

;; A MatchResult is a (match-result (or PatternEnv #f) Progress (Listof Expr))
;; penv : (or PatternEnv #f) - the pattern env on success, #f on failure
;; progress : Progress - how far matching got (meaningful on failure)
;; ose-exprs : (Listof Expr) - ~var subexpressions accumulated regardless of success/failure
(struct match-result [penv progress ose-exprs] #:transparent)

;; match-result-success? : MatchResult -> Boolean
(define (match-result-success? r)
  (and (match-result-penv r) #t))

;; try-patterns : [Listof Pattern] Expr -> (or PatternEnv (Listof (Listof Expr)))
;; Tries each pattern in order until one matches.
;; On success, returns the PatternEnv.
;; On failure, returns a list of OSE expr lists from the pattern(s) that made the most progress.
(define (try-patterns patterns expr)
  (define results
    (for/list ([pat patterns])
      (match-top-pattern pat expr)))
  (define success
    (for/first ([r results] #:when (match-result-success? r))
      (match-result-penv r)))
  (cond
    [success success]
    [else
     (define failures
       (sort results
             (lambda (a b)
               (progress<? (match-result-progress a)
                           (match-result-progress b)))))
     (cond
       [(null? failures) (list)]
       [else
        (define most-progress (match-result-progress (last failures)))
        (for/list ([f failures]
                   #:when (equal? (match-result-progress f) most-progress))
          (match-result-ose-exprs f))])]))

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

;; match-top-pattern : Pattern Expr -> MatchResult
;; Matches a top-level pattern against syntax.
;; The car of both pattern and syntax is the macro name (ignored per syntax-rules semantics).
(define (match-top-pattern pat expr)
  (match* (pat expr)
    [(`(,_ . ,pd) `(,_ . ,ed))
     (match-pattern pd ed (list 'rest))]
    [(_ _) (match-result #f (list) (list))]))

;; match-pattern : Pattern Expr Progress-Rev -> MatchResult
;; Matches a pattern against syntax.
;; Always returns a MatchResult. On success, penv is a hash. On failure, penv is #f.
;; Continues matching both sides of a pair even after one side fails,
;; so that ~var subexpressions are collected from the entire pattern.
(define (match-pattern pat expr progress-rev)
  (match* (pat expr)
    ;; ~var: always matches, records expr for OSE
    [(`(~var ,pvar) expr)
     (match-result (hash pvar expr) (reverse progress-rev) (list expr))]
    ;; regular variable: always matches
    [((? symbol? pvar) expr)
     (match-result (hash pvar expr) (reverse progress-rev) (list))]
    ;; pair: recurse into both sides, continue even if one fails
    [(`(,pa . ,pd) `(,ea . ,ed))
     (define ra (match-pattern pa ea (cons 'first progress-rev)))
     (define rd (match-pattern pd ed (cons 'rest progress-rev)))
     (define ose (append (match-result-ose-exprs ra) (match-result-ose-exprs rd)))
     (cond
       [(not (match-result-success? ra))
        (match-result #f (match-result-progress ra) ose)]
       [(not (match-result-success? rd))
        (match-result #f (match-result-progress rd) ose)]
       [else
        (define merged
          (hash-union-safe (match-result-penv ra) (match-result-penv rd)))
        (if merged
            (match-result merged (reverse progress-rev) ose)
            (match-result #f (reverse progress-rev) ose))])]
    ;; literal: must be equal
    [(_ _)
     (if (equal? pat expr)
         (match-result (hash) (reverse progress-rev) (list))
         (match-result #f (reverse progress-rev) (list)))]))

;; hash-union-safe : Hash Hash -> (or Hash #f)
;; Merges two hashes. Returns #f if any shared key has conflicting values.
(define (hash-union-safe a b)
  (let/ec return
    (hash-union a b
                #:combine (lambda (va vb)
                            (if (equal? va vb) va (return #f))))))

(module+ test
  (require rackunit)

  ;; ---- progress<? tests ----
  (check-true (progress<? '() '(first)))
  (check-true (progress<? '(first) '(first first)))
  (check-false (progress<? '(first first) '(first)))
  (check-false (progress<? '(first) '(first)))
  (check-true (progress<? '(first) '(rest)))
  (check-false (progress<? '(rest) '(first)))
  (check-true (progress<? '(first rest) '(rest first)))
  (check-false (progress<? '(rest first) '(first rest)))

  ;; ---- match-top-pattern tests ----
  ;; success: plain variable
  (check-equal? (match-result-penv (match-top-pattern '(m p) '(m e)))
                (hash 'p 'e))
  ;; failure: too few args
  (check-equal? (match-top-pattern '(m p) '(m))
                (match-result #f '(rest) '()))
  ;; failure: literal mismatch
  (check-equal? (match-top-pattern '(m 1) '(m 2))
                (match-result #f '(rest first) '()))
  ;; failure: deep literal mismatch
  (check-equal? (match-top-pattern '(m ((1))) '(m ((2))))
                (match-result #f '(rest first first first) '()))

  ;; ---- ~var tests ----
  ;; success with ~var
  (check-true (match-result-success? (match-top-pattern '(m (~var x)) '(m e))))
  (check-equal? (match-result-penv (match-top-pattern '(m (~var x)) '(m e)))
                (hash 'x 'e))
  (check-equal? (match-result-ose-exprs (match-top-pattern '(m (~var x)) '(m e)))
                '(e))
  ;; success with ~var in nested position
  (check-true (match-result-success? (match-top-pattern '(m (1 (~var x))) '(m (1 hello)))))
  (check-equal? (match-result-penv (match-top-pattern '(m (1 (~var x))) '(m (1 hello))))
                (hash 'x 'hello))
  (check-equal? (match-result-ose-exprs (match-top-pattern '(m (1 (~var x))) '(m (1 hello))))
                '(hello))
  ;; failure with ~var: literal mismatch but ~var still collected
  (check-equal? (match-top-pattern '(m (1 (~var x))) '(m (2 hello)))
                (match-result #f '(rest first first) '(hello)))
  ;; multiple ~var success
  (check-true (match-result-success? (match-top-pattern '(m (~var a) (~var b)) '(m x y))))
  (check-equal? (match-result-penv (match-top-pattern '(m (~var a) (~var b)) '(m x y)))
                (hash 'a 'x 'b 'y))
  (check-equal? (match-result-ose-exprs (match-top-pattern '(m (~var a) (~var b)) '(m x y)))
                '(x y))
  ;; ~var before failing literal: ~var expr collected
  (check-equal? (match-top-pattern '(m (~var a) 1) '(m x 2))
                (match-result #f '(rest rest first) '(x)))
  ;; ~var after failing literal: ~var expr still collected (we continue matching)
  (check-equal? (match-top-pattern '(m 1 (~var a)) '(m 2 x))
                (match-result #f '(rest first) '(x)))

  ;; ---- try-patterns tests ----
  ;; all fail, no ~var: empty OSE lists
  (check-equal? (try-patterns '((m 1) (m 2)) '(m 3))
                '(() ()))
  ;; second pattern has more progress
  (check-equal? (try-patterns '((m 1) (m (1 2))) '(m (1 3)))
                '(()))
  ;; first pattern has more progress
  (check-equal? (try-patterns '((m (1 2)) (m 1)) '(m (1 3)))
                '(()))
  ;; success short-circuits
  (check-equal? (try-patterns '((m x)) '(m 42))
                (hash 'x 42))
  ;; ~var collects OSE exprs on failure
  (check-equal? (try-patterns '((m (1 (~var x)))) '(m (2 hello)))
                '((hello)))
  ;; tie with ~var: both patterns fail at same depth
  (check-equal? (try-patterns '((m (1 (~var a))) (m (1 (~var b)))) '(m (2 foo)))
                '((foo) (foo)))
  ;; ~var matches everything, so (m (~var x)) succeeds — try-patterns returns penv
  (check-equal? (try-patterns '((m 1) (m (~var x))) '(m 3))
                (hash 'x 3))

  ;; ---- structural mismatch: pat is pair, expr is atom, ~var in pat tail ----
  ;; pattern (m (1 (~var x))) vs (m 5): expr 5 is not a pair, but ~var is in the pattern
  ;; We should still collect the ~var... except there's no corresponding subexpr to bind.
  ;; Actually the real bug: (m p (~var x)) vs (m 5) — pat-cdr is (p (~var x)), expr-cdr is (5).
  ;; Then pa=p ea=5 succeeds (variable), pd=((~var x)) ed=() — structural mismatch,
  ;; pd is a pair but ed is not. The ~var inside pd is lost.
  (check-equal? (match-top-pattern '(m p (~var x)) '(m 5))
                (match-result #f '(rest rest) '()))
  ;; Now the real case: both sides are pairs but pa fails, pd has ~var
  ;; (m 1 (~var x)) vs (m 2 hello): pa=1 ea=2 fails, pd=((~var x)) ed=(hello)
  ;; We already test this above and it works. Let's test the deeper case:
  ;; (m (1 (~var a)) (2 (~var b))) vs (m (9 foo) (9 bar))
  ;; pa=(1 (~var a)) ea=(9 foo): 1 vs 9 fails at rest>first>first, ose=(foo)
  ;; pd=((2 (~var b))) ed=((9 bar)): 2 vs 9 fails at rest>rest>first>first, ose=(bar)
  ;; Both sides fail. Progress should be from FIRST failure (pa): rest>first>first
  ;; OSE should include BOTH: (foo bar)
  (check-equal? (match-top-pattern '(m (1 (~var a)) (2 (~var b))) '(m (9 foo) (9 bar)))
                (match-result #f '(rest first first) '(foo bar)))

  ;; pa succeeds, pd fails, both have ~var — progress from pd, all OSE collected
  ;; (m (~var a) (2 (~var b))) vs (m foo (9 bar))
  ;; pa=(~var a) ea=foo: succeeds, ose=(foo)
  ;; pd=((2 (~var b))) ed=((9 bar)): 2 vs 9 fails at rest>rest>first>first, ose=(bar)
  ;; Progress from pd failure. OSE = (foo bar)
  (check-equal? (match-top-pattern '(m (~var a) (2 (~var b))) '(m foo (9 bar)))
                (match-result #f '(rest rest first first) '(foo bar)))

  ;; pa fails, pd succeeds with ~var — progress from pa, all OSE collected
  ;; (m (2 (~var a)) (~var b)) vs (m (9 foo) bar)
  ;; pa=(2 (~var a)) ea=(9 foo): 2 vs 9 fails at rest>first>first, ose=(foo)
  ;; pd=((~var b)) ed=(bar): succeeds, ose=(bar)
  ;; Progress from pa failure. OSE = (foo bar)
  (check-equal? (match-top-pattern '(m (2 (~var a)) (~var b)) '(m (9 foo) bar))
                (match-result #f '(rest first first) '(foo bar)))

  ;; ---- multi-clause + ~var tests ----
  ;; first clause makes more progress, has ~var
  ;; (m (1 (~var a) 2)) fails at rest>first>rest>rest>first matching 2 vs 3
  ;; (m 1)              fails at rest>first matching 1 vs (1 foo 3)
  (check-equal? (try-patterns '((m (1 (~var a) 2)) (m 1)) '(m (1 foo 3)))
                '((foo)))
  ;; second clause makes more progress, has ~var
  ;; (m 1)              fails at rest>first matching 1 vs (1 foo 3)
  ;; (m (1 (~var a) 2)) fails at rest>first>rest>rest>first matching 2 vs 3
  (check-equal? (try-patterns '((m 1) (m (1 (~var a) 2))) '(m (1 foo 3)))
                '((foo)))
  ;; first clause deeper, second clause has ~var but less progress — only first reported
  ;; (m (1 2))    fails at rest>first>rest>first matching 2 vs 3
  ;; (m (~var a)) succeeds — so try-patterns returns penv
  (check-equal? (try-patterns '((m (1 2)) (m (~var a))) '(m (1 3)))
                (hash 'a '(1 3)))
  ;; both fail, first deeper with ~var, second shallower without
  ;; (m (1 (~var a) 2)) fails at rest>first>rest>rest>first
  ;; (m (9))            fails at rest>first>first
  (check-equal? (try-patterns '((m (1 (~var a) 2)) (m (9))) '(m (1 foo 3)))
                '((foo)))
  ;; both fail, second deeper with ~var, first shallower without
  (check-equal? (try-patterns '((m (9)) (m (1 (~var a) 2))) '(m (1 foo 3)))
                '((foo)))

  ;; ---- ~var inside sublist pattern ----
  ;; ~var nested inside a sublist, match succeeds
  (check-equal? (match-result-penv (match-top-pattern '(m ((~var x))) '(m (hello))))
                (hash 'x 'hello))
  (check-equal? (match-result-ose-exprs (match-top-pattern '(m ((~var x))) '(m (hello))))
                '(hello))
  ;; ~var inside sublist, outer literal fails — ~var still collected
  (check-equal? (match-top-pattern '(m 1 ((~var x))) '(m 2 (hello)))
                (match-result #f '(rest first) '(hello)))
  ;; ~var inside sublist, sibling inside sublist fails
  ;; pattern (m (1 (~var x))) vs (m (2 hello)): 1 vs 2 fails at rest>first>first
  (check-equal? (match-top-pattern '(m (1 (~var x))) '(m (2 hello)))
                (match-result #f '(rest first first) '(hello)))
  ;; multiple ~var inside nested sublists
  (check-equal? (match-result-penv
                 (match-top-pattern '(m ((~var a)) ((~var b))) '(m (x) (y))))
                (hash 'a 'x 'b 'y))
  (check-equal? (match-result-ose-exprs
                 (match-top-pattern '(m ((~var a)) ((~var b))) '(m (x) (y))))
                '(x y))
  ;; deeply nested ~var inside sublist, failure elsewhere
  ;; pattern (m (1 ((~var x))) 2) vs (m (1 ((hello)) 3)): 2 vs 3 fails at rest>first>rest>rest>first
  (check-equal? (match-top-pattern '(m (1 ((~var x)) 2)) '(m (1 ((hello)) 3)))
                (match-result #f '(rest first rest rest first) '((hello))))
  ;; try-patterns: two clauses, both with ~var in sublists, tied progress
  ;; (m (1 (~var a))) fails at rest>first>first
  ;; (m (1 (~var b))) fails at rest>first>first — same progress, both reported
  (check-equal? (try-patterns '((m (9 (~var a))) (m (9 (~var b)))) '(m (1 foo)))
                '((foo) (foo)))
  ;; try-patterns: ~var in sublist, first clause deeper
  ;; (m ((1 (~var a)) 2)) fails at rest>first>rest>first matching 2 vs 3
  ;; (m (9))              fails at rest>first>first matching 9 vs 1
  (check-equal? (try-patterns '((m ((1 (~var a)) 2)) (m (9))) '(m ((1 foo) 3)))
                '((foo)))
  )
