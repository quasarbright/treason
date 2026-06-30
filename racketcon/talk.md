title:
Treason: Making macros and IDE services work together
description:
Racket's macros let us extend the language and create DSLs, but they also get in the way of providing good IDE services when the program is broken or incomplete. Treason is a prototype of a macro-extensible language that provides good IDE services even when such errors prevent complete macro expansion. Treason's macro expander recovers from errors to continue expanding and collecting information used to drive IDE services. Our key contribution is __spec-driven subexpression expansion__: syntax-class annotations allow us to expand subexpressions even within a broken macro use.

what to write about
- objective of project, cool stuff like ose, what you get from it
- why we chose stx parse progress
- is stx parse progress the right way? where it fails with SSE (prefix-centric)
- clearly distinguish between "what we made and why" vs open research questions we aren't sure about
- backtracking gets weird with SSE (open questions?)
- show off all the cool stuff, why it's that way, and the benefits
- enumeration of possible errors, where they arise, and when we can and cannot provide services/ose. like extra expr at beginning middle end, missing expr at beginning middle end, wrong expr anywhere.
- cool stuff
	- services on patterns
	- services on templates
	- multiple errors, fault tolerance. already done in other languages
	- ose, even with ellipses. services even in the face of stx error. this is novel
	- IDE-focused


feedback from michael
- [ ] read vs expand errors. no parens, you're screwed and the error may be somewhere far away (we don't solve that currently but maybe put it in future work)
- [x] talk about how hygiene interacts with all of this. getting a little into impl maybe. don't talk about scope graphs
	- not sure how to do this without getting more into impl. maybe best left to impl discussion
- [x] make benefits more of a demo thing and don't go into as much detail. go into more detail later. just show off the nice things at first

feedback from michael 2
- [x] we know drracket sucks and other languages have better services. but we accept this because it's impossible to have good IDE services with such an expressive macro-extensible language, right?????
- [x] background about other languages before we talk about treason. break out ts stuff or rust into a background section. how they do it. maybe don't bring up any bc that's type stuff and this has nothing to do with types. mention types in future work? like if you're defining a fault tolerant typed dsl you want to be able to do your own sentinels like any.
- [x] for rewriting fault tolerance: (after talking about how ts and even rust just keep going) basic fault tolerance like with definitions is easy. we can just copy that, which is what rust and lean do. macros don't make this hard, we can just keep going. but what about services within a bad macro use site? that's where the opacity of macros makes this difficult, and where SSE comes in. And this is our main novel contribution

- [x] put this in the repo
- [ ] shorten limitations to the high level, but keep them somewhere for ourselves. want to make this exciting for future work and not sad like treason sucks lol.
- [ ] maybe combine limitations, open questions, future work, maybe just combine OQ and FW
- [ ] talk more about the vision for syntax-spec + Treason. like a whole example and "what do you do with SS if something goes wrong?"
---
## What is Treason?
Treason is a Racket-like language with better IDE support.
## Intro
Listen, we all know Racket doesn't exactly have the best IDE experience. But we're ok with that because that's the cost of having such an expressive language. It's impossible to have a language as expressive as Racket, but with IDE services on par with mainstream languages. Right? Well actually, it is possible, and I'll show you how.
## Background
In Racket, if there is an expansion error in a file, like an unbound variable or a misuse of a macro, that error is reported and expansion stops. If there are other issues in the file after that error, they are not reported, and we don't get IDE services on parts of the program that would get expanded after the site of the error since the expander gave up. Language servers can cache the static knowledge gathered by a previous successful expansion, or even static knowledge gathered before the error was encountered, but this leads to missing or out-of-date information being reported by IDE services.
When a program expands without issue, the language server works fine and we get a nice IDE experience with most of the things people expect, such as auto-complete, go to definition, and documentation on hover.
However, most of the time, when writing a program, it is not in a syntactically valid state. Here are some common examples:
```racket
;; Real Real -> Real
;; Magnitude of a 2D vector represented by its x and y coordinates
(define (magnitude x y)
  (define x2 (sqr x))
  (define y2 (sqr y))
  (define mag-sq (+ x2 y2))
  )
```
Often times, we build up some temporary variables to split up a computation into steps. However, in the body of a definition, a `let`, a `match` clause, or any other similar form, the last form must be an expression. Of course, we are about to finish off this function by adding `(sqrt mag-sq)` as the final form of the body. But since the program has an error, and has had one since we started implementing this function, we will not get full IDE services while implementing the body. Racket's VSCode extension can leverage static knowledge cached from a past file state that expanded without issue, but will not have any of our temporary variables in the autocomplete since their bindings were never recorded in a successful expansion.
Another, similar problem is seen when writing functions top-down, which I assume we all do as followers of how to design programs!
```racket
;; A Grade is a Number in [0, 100]

;; A Letter is one of "A" "B" "C" "D" "F"

;; A Student is a structure:
(struct student (name grades) #:transparent)
;; - name   : String
;; - grades : [List-of Grade]

;; A Report is a structure:
(struct report (name average letter) #:transparent)
;; - name    : String
;; - average : Grade
;; - letter  : Letter

;; student->report : Student -> Report
;; Builds a Report from a single Student.
(define (student->report s)
  (define avg (average-grade (student-grades s)))
  (report (student-name s)
          avg
          (grade->letter avg))
```
We use helper functions like `average-grade` and `grade->letter` before they are defined. We will have unbound variable errors at least until we have stubs for all of those, again leading to a lack of IDE services, or at the very least, the inconvenience of having to implement these stubs first to get services.
Clearly, the design of immediately giving up on the first error encountered during expansion hinders IDE services.
In other languages like Rust (which has macros!), the compiler does not give up after the first error, and we even get IDE services after errors.
![[Pasted image 20260610201130.png]]

Not only do we get services on subsequent code, we even get autocomplete on those bad definitions!
![[Pasted image 20260610203340.png]]
However, when we do use typical lightweight macros in Rust, we don't get the same benefits. Why?
This is possible with standard Rust code because the Rust compiler knows the grammar and static semantics of Rust, and can figure out roughly what you probably meant when things go a little wrong.
Macros are tricky because they're opaque syntax to syntax transformations, so the compiler doesn't know much about their grammars. And if they aren't fault-tolerant, there is no way for the compiler to make them fault-tolerant in general.
But in Racket, everything's a macro! So that's why we get the poor services. The first step of the solution is to just keep going after errors like Rust, which is relatively easy. What's harder is dealing with an error inside of a bad macro use gracefully, and continuing to get services in other parts of that bad use. That's the main novel contribution of treason. We'll get to that later, but first, let's see what Treason looks like.
## Treason Demo
In Treason, we get IDE services even when there are expander errors.
![[Pasted image 20260521202441.png]]
This program has an unbound variable, but we still get autocomplete later on and the variable `y` with an invalid definition is included!
We also get autocomplete in positions where an expression is missing.
![[Pasted image 20260529105419.png]]
Here, the body of the `my-let` is missing, which would normally just be a syntax error. If we run autocomplete from that position, not only does it work, but we even get `y` included!
We also get IDE services on subexpressions of macro uses, even when the use is invalid.
![[Pasted image 20260529102934.png]]
We have a malformed binding group in the outer `my-let`, so it's a syntax error. However, we still get services inside the inner `my-let` because Treason knows that the body of a `my-let` is just an expression and not some special part of the macro.
Another nice little feature is that we get IDE services in the template of a macro definition:
![[Pasted image 20260521211622.png]]
We get autocomplete in the template, which of course includes pattern variables like `m` and `p`, but also the macro-introduced binding of `x`. And again, in an empty `let` body!
Now that we know what Treason gives us, let's talk about how it works.
## Fault Tolerance
Treason was designed with IDE services in mind, and the key divergence from Racket and other similar languages is fault-tolerant expansion.
In Treason when the expander encounters an error like a bad macro use or an unbound variable, the bad syntax is replaced by a sentinel node indicating the error and expansion continues. The implementation of this is relatively easy, and languages like Rust already do it. Simple fault-tolerance alone greatly increases the availability of IDE services while writing a program with syntax errors, as we've already seen.
![[Pasted image 20260521202441.png]]
## Spec-driven Subexpression Expansion
Getting services after a bad macro use is easy with sentinels. The harder part is getting services _inside_ of a bad macro use.
```racket
(define-syntax my-define
  (syntax-rules ()
    [(my-define (f:id x:id ...) (~var body expr) ...)
     (define f (lambda (x ...) (block body ...))]))

(my-define (f 2)
            ; ^ bad!
  lots of exprs here)
```
In this example, we have a bad parameter in our function definition, but the body is fine. Normally in Racket, if a syntax-parse pattern fails to match on part of a macro use, it immediately gives up and we don't emit any syntax from the macro or expand its subexpressions. Macros are a syntax-to-syntax transformation, so if the input is invalid, we might not be able to generate the output, so we'd have no emitted syntax to expand. Sometimes that's fine, and a bad macro use is completely beyond saving. But in cases like this, it's obvious that there are lots of plain old expressions in there that we want services on.

We can't expand the macro, but we do know the rules of the subexpressions from the annotations, so we can expand those. This allows us to get services on the subexpressions from the information we got during expansion. I call this feature spec-driven subexpression expansion, or SSE for short, and it is the main novel contribution of Treason.

![[Pasted image 20260529102934.png]]
However, in this example, notice that `x` is not included in the autocomplete. When the pattern fails to match, we never generate any macro-introduced code, so we never find out that the `x` is supposed to be bound. So the best we can do is expand the subexpression alone in the same context as the macro use to get the not-fully-accurate static information.
If we were somehow able to indicate the binding rules of a macro, declaring that `x` is in scope in the `my-let` body, we could get `x` in the autocomplete even in this misuse because the expander would know that `x` is supposed to get bound. In fact, if we knew the grammars and binding rules for all macros, we could theoretically get full services on a program without expanding any phase 0 macro uses. These kinds of declarations are supported in a tool called syntax-spec, which I worked on with Michael Ballantyne. In the future, we could add something like syntax-spec to Treason to support this.
If a macro has more than one clause, SSE gets more complicated. For example:
```racket
(define-syntax m
  (syntax-rules ()
    [(m 1 (~var e expr)) 1]
    [(m (~var e expr) 2) 2]))
(m (let ([x 3]) x) (let ([y 4]) y))
```
Should we get services on both subexpressions? Just the first? Just the second?
Treason uses a mechanism similar to what's used in `syntax/parse` to determine which clause made the most progress from left to right and out to in. We take this clause and use its annotations to decide which source subexpressions to expand.
To identify which subexpressions correspond to annotated pattern variables, we go left to right and outside in. As a consequence, if you have a macro like this
```racket
(define-syntax m
  (syntax-rules ()
    [(m 1 2 (~var e expr)) 1]))
(m 1 (let ([y 4]) y))

```
And you forget the 2, the `let` will be interpreted as if it's supposed to be the 2, and will not get expanded so we won't get services on it. The `e` will be interpreted as missing. In general, Treason doesn't try to tell the difference between a missing, extra, or incorrect subexpression, and just assumes there is nothing missing or extra as far as SSE is concerned. So for positionally sensitive macros, bad uses that have missing or extra subexpressions can "misalign" SSE and cause it to run on the wrong subexpressions.
## Services in Templates
Another, small nice thing is that we get services in templates
![[Pasted image 20260521211622.png]]
Inside the template, we get `m` and `p` in the autocomplete, which isn't surprising. But we also get the macro-introduced `x`! Currently, this only works if the macro is used somewhere, since it works by recording the resolutions during the expansions of macro uses. Since this includes resolution of macro-introduced syntax which came from the template, expansion of a use collects the information needed to get services in the template.

This does require a macro use to work, and this is a deliberate tradeoff. It would be possible to get services in templates without macro uses if we eagerly expand templates. But making that work would restrict the expressivity of the macro system, so we choose to require uses for services in templates to maintain expressivity. And if you're a good how to design programs student, you should be writing uses before you implement your macro anyway!
## Hygienic Autocomplete
![[Pasted image 20260529105419.png]]
In Treason, we get autocomplete even when the expression at your cursor is missing and there is a syntax error. Fault-tolerance via sentinel values does a lot of the work for us, but if we want that `y` in the autocomplete, we're somehow going to need to expand the program and figure out what variables should be in scope at the cursor. We do this by inserting a special cursor identifier at the cursor point, expanding the program, and seeing what names that cursor identifier can hygienically resolve to when it expands. Normally, variable resolution checks the scope marks on an identifier and compares it to the scope marks on bindings to see if the variable can hygienically resolve to a binder. Here, we flip this around and search for all of the binders which the identifier can hygienically resolve to.
If we were in a language without macros, we could avoid doing the identifier insertion and we could just check what's in scope at the surrounding form. But hygienic macros require this kind of thing to get hygienic autocomplete right in general.
## Limitations
Treason helps give us IDE services in some situations where languages like Racket do not. However, there are some limitations and drawbacks to the approach we took, as well as features that just haven't been implemented yet.
### SSE leads to expansion in the wrong context
One important drawback with fault-tolerance is that expansion can happen on invalid programs and in unexpected contexts, which can cause unexpected behavior. For example,
![[Pasted image 20260529093204.png]]
In this example of SSE, the body of the `my-let` expands to get services on it, even though the binding group is invalid. This body is a reference to the improperly-bound `x`. Since this binding was never registered, it looks like `x` is unbound, so we get a somewhat confusing error. This could be alleviated by saying something like "unbound identifier inside of a misuse of a macro, which may be the real problem". But in general, having expressions expand outside of the proper context can cause even more unexpected behavior. We may want to add some hooks to allow macro authors to disable SSE in certain situations to prevent this.
### Side Effects
Another unfortunate interaction with macro side effects is autocomplete. For example,
![[Pasted image 20260529093929.png]]
Here, we get `x` in autocomplete despite the `let` missing its body. In the case where the user's cursor is not in an existing identifier, we achieve by inserting a special identifier where the cursor is, expanding the program, and seeing what names were in scope when the cursor identifier was resolved. When we do this, we must re-expand the entire program, which is both inefficient and could lead to extra side effects during expansion.
In general, the goals of treason do have some friction with effect-ful macros, and this is still an open problem.
Efficiency is a limitation here. We will keep expanding when we see an error, which could waste time trying to expand a garbage program, and we re-expand for autocomplete in some situations. We also re-parse and re-expand the entire program on every edit. We could do tree-sitter for incremental parsing and something like salsa for incremental expansion, but we haven't tried that yet.
### Fault-tolerant Reading
One other thing that hasn't been implemented yet is fault-tolerant reading. When we process a treason file, we first "read" it by parsing it into s-expressions, and then we treat this s-expression as the surface syntax of a program when we expand it. Expansion is fault-tolerant and continues after errors, but reading is currently not fault-tolerant. For example, if we have an unexpected close-paren, parsing immediately gives up and expansion never happens. We could use indentation to infer where parentheses should be and return a sentinel node on parse failure, but that hasn't been implemented yet.
### Procedural Macros
We also haven't implemented procedural macros, which could get messy for a few reasons. Right now, our pattern-based macro language is baked into the compiler and the annotations that power SSE are similarly baked in. To support procedural macros, we'd want to expose hooks to allow macro authors to control spec-driven subexpression expansion. Procedural macro authors would also have to account for the possibility of sentinel nodes being present in partially expanded code if we support an operator like `local-expand`. And as we've discussed, authors of macros with side effects must account for fault-tolerant expansion and potential expansion of prospective programs like the case of cursor-insertion autocomplete. In general, increased availability of IDE services seems to have the drawback of increased burden on macro authors. We are trying to design treason to minimize these burdens, but there is definitely more friction than in a language without fault-tolerant expansion.
### Recursive Macros
Another limitation with the current implementation is that recursive macros don't work as well with fault-tolerance and SSE, so whether a user gets IDE services in a region of a program currently depends on how the macros are implemented internally. For example,
![[Pasted image 20260529100314.png]]
Here we have a recursive implementation of `let*`. We recur on the sequence of binding pairs. In the recursive case, we only declare that the first binding pair must be a pair, and we rely on recursion to enforce the structure of the rest of the binding pairs. However, the recursive case is only invoked when the initial pattern succeeds to match since it relies on expanding the generated recursive call. In our case, the recursive pattern fails to match since the binding group is empty, and we never "look inside" of the other binding pair, so we never realize that the second expression should be expanded, and we get no services on the right-hand-side of the `x` binding. We can alleviate this by creating a more precise pattern:
![[Pasted image 20260529101805.png]]
Now we get services on the second binding pair.
When writing macros, it is best to validate syntax as early as possible with precise patterns to get the most precise error messages. This is not only true in Treason, but also in Racket with `syntax/parse` as well.
### Services in templates require use
We get services in templates, but only when the macro is used. Without having the template expand, there is currently no way to know which variables end up as definitions and references. With something like syntax-spec, we could get these services without having to expand.

### Single-file (no modules)
There is no module system, so programs are only a single standalone file. However, I imagine our approach will generalize to a language with multiple modules without significant challenges.
## Open questions
- progress like syntax parse?
- how to deal with side effects?
- could any of this be retrofitted to Racket?
	- basic fault tolerance maybe could. like add an optional flag to enable fault-tolerant local-expand? valid programs would have no behavioral change, only invalid ones.
## Future work
- syntax spec to get even more services and static information
	- no more use requirement for template services
	- can do SSE-like thing with bindings like in `(my-let ([x]) HERE)` getting `x` in auto-complete despite invalid binding group
	- way less of a need to expand to get services
- procedural macros
	- side effects
	- procedural hook for SSE and creating an error sentinel
	- to bootstrap, need something like syntax-local-value and a sophisticated reflected environment for ellipses and quote
		- syntax-local-value successful lookup registers a reference with LSP. include success predicate for whether to record reference, like binding class predicate if the lookup is looking for a particular type of variable.
	- quotes and quasiquotes
	- local expand would get weird with sentinels
	- phasing
- hash lang for racket interop