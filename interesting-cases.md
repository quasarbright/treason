# Interesting Cases

interesting cases to think about as we change things

## getting expanded under multiple environments

```racket
(let-syntax-rule ([(m body) (if 1 
                                (let ([x 2]) (let ([y 2]) body))
                                (let ([y 2]) (let ([z 2]) body)))])
  (m _))
```
what should auto-complete show at the underscore? just y?

with hygiene

```racket
#lang racket

(define x 5)

(define-syntax bind-a-or-b
  (syntax-rules ()
    [(_ #t a b)
     (define a 6)]
    [(_ #f a b)
     (define b 7)]))

(define-syntax ref-a-or-b
  (syntax-rules ()
    [(_ #t a b)
     a]
    [(_ #f a b)
     b]))

(define-syntax-rule (m bool)
  (begin
    (bind-a-or-b bool y z)
    _1
    (ref-a-or-b bool _2 z)
    ))

(m #t)
(m #f)
```
What should be in completions at `_1` and `_2`? With conditional binding, you can't know and it's undecidable.