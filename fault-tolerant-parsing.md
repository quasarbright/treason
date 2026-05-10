# Fault-tolerant Parsing

Want to be able to continue parsing and even expand programs with parser errors, like bad parens.
Bad paren fault-tolerance will likely be powered by something like parinfer.

Property: If a program parses correctly, we shouldn't change it.

left off looking at tree sitter, not sure if it will be the right answer. idk if it can do indentation-based inference

## missing close paren

```clojure
(begin
  (define (f x) 2 ; forgot to close this paren
  (define y 3))

(define z 4)
```

parses like

```clojure
(begin
  (define (f x) 2 (define y 3))
  (define z 4) ; missing ) error
```
Doing parinfer, if we get rid of end-of-line close parens and insert them at out-dents (or same-dents)

```clojure
{begin
  {define (f x) 2
  }{define y 3

}}{define z 4}
```
which becomes
```clojure
(begin
  (define (f x) 2)
  (define y 3))

(define z 4)
```

## mismatch

use close paren instead of close square brace

```clojure
(let ([x 2)
      [y 2])
  x)
```

just replace it with a square brace?

but then what if you just forget it completely?

```clojure
(let ([x 2
      [y 2])
  x)
```
this would become
```clojure
(let ([x 2
      [y 2]] ; mismatch ) ~> ]
  x)
```
which parses like
```clojure
(let ([x 2 [y 2]]
      x) ; missing ) error
```
and gets inferred like
```clojure
{let {{x 2
      }{y 2
  }}x}
```
which is correct

But we don't want to parinfer the whole file. Maybe only parinfer from the unmatched open to eof

But then valid stuff with weird indentation will parse differently

Also,

```clojure
([([(])])
```

Really, this is JUST an unmatched open paren. The current parser will say single mismatch and give up, which is fine. But with replacements as we go, we'll get a mismatch error on every closer since they'll all be off by one, and then a missing close paren at eof. So we get more parse errors by doing replacements.

Could use "poisoning" to prevent extra errors by poisoning the surrounding node and only reporting the "first" error of a poisoned node.

Alternative: on mid-line issue like mismatch or extra close, give up until next outdent/same-dent?