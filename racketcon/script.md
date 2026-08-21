# Treason talk script

Slide-transition markers (`▶`) mark moving to the next slide. Reveals within a
slide aren't marked.

---

**▶ Title**

Hi, I'm Mike Delmonaco. I'm a software engineer at Amazon Web Services, and I went to Northeastern, where I learned Racket. And these days I do a little bit of programming language research on the side with Michael Ballantyne. Today I want to show you something Michael Ballantyne and I have been working called Treason. It's a language with a macro system similar to Racket's, but with better IDE support, even when the program is broken.

**▶ Agenda**

Before I get started, here's an outline of what I'm going to talk about. First, I'll show you the weaknesses of Racket's IDE experience. Then, I'll show you how some other languages with macros have already overcome these challenges, but are still limited. Next I'll show off how treason surpasses these languages using a new technique we developed called spec-driven subexpression expansion, and explain how it all works. Finally, we'll talk about some limitations of our approach and how we can take it even further in the future.

**▶ Racket Gives Up on the First Error**

First thing's first, let's talk about Racket. One weakness of Racket is its poor IDE experience compared to other languages. If there is an expander error anywhere in your file, your IDE experience will suffer. No autocomplete, no go to definition, no find references. And you only ever see the first problem in a program even if there are many.

**▶ You have an error most of the time (magnitude)**

Unfortunately, most of the time, you DO have an error while you're writing your program. Here's a typical example. I'm writing a magnitude function. I've got my temporary variables, x squared, y squared, the sum of those, and I'm about to finish it off with `(sqrt mag-sq)`. But I haven't typed that yet. And because the body of a `define` has to end in an expression, this whole thing is an error. I get a red squiggly saying "the last form is not an expression" until I add it. And since expansion stops right here, at this error, I get no services for the rest of the file after this line.

**▶ You have an error most of the time (student->report)**

Here's another one. Like good htdp students, we write our functions top-down. Here i'm implementing `student->report`, and I'm calling helper functions, `average-grade` and `grade->letter`, which I haven't written yet. Those are unbound, so I get no services until I go stub out every single helper, which is annoying. I just want to write the function top-down and have the tooling keep up with me.

**▶ A Tradeoff?**

Surely, this is unavoidable. You can extend the language however you want, so of course the IDE can't know as much about your programs. You can't have a language with macros AND also get good IDE services. Right?

Actually, you can. 

**▶ Other Languages Keep Going (Rust)**

Other languages with macros like Rust already have better IDE services than Racket. The Rust compiler doesn't stop at the first error. It keeps going, and you still get services on the code after the error. And despite the fact that both of these definitions are broken since they reference something unbound, Rust still gives you autocomplete with the broken `bad` in the suggestions. Pretty nice! Let's see what happens when we throw macros in the mix.

**Macro example**

Here is an example of a macro in rust that allows you to write a json map with nice syntax. We have an arrow in between each field name and value. But if we pull that status out into a variable and try to type it into the macro, we don't get autocomplete. And if we write those arrows wrong, we get a syntax error. But only the first error! What happened? I thought Rust had good IDE services and never gave up?

**▶ But Not Rust Macros (why)**

What's going on here is, plain Rust works well because the compiler knows Rust's grammar and its static semantics, so when something goes a little wrong, it can figure out what you probably meant since plain Rust is totally baked in. But the compiler doesn't know as much about your macros since they're opaque syntax to syntax transformations. And if the macro itself isn't written to be fault-tolerant, there's no way for the compiler to make it fault-tolerant for you, which is why we only see the first error in a bad use. Just like Racket. This lack of fault tolerance is all over Racket because EVERYTHING is a macro! That's part of why the services aren't as good.

**▶ The fix**

What we need is some way to help the expander understand your macro's static semantics by making is less opaque. If we look at pattern-based macros, there is plenty of information just begging to be used by the expander. The pattern gives you a rough idea of the grammar, and if you use syntax-parse, you're probably already used to annotating pattern variables with syntax classes, which gives us even more information about the grammar. In Racket, this is pretty much only used for producing good error messages. But if we make the most of these annotations, we can use them to get services even in a bad macro use. This is the main novel contribution of Treason.

**▶ What is Treason?**

Like I said before, Treason is a Racket-like language with better IDE support. Here's an example: We have an unbound variable in the right-hand-side of two definitions. In Racket, we only saw the first error and we got no services on the whole file. But in Treason, we see both errors, we get services like autocomplete, and autocomplete even includes variables whose definitions failed to expand!

**▶ Demo: Autocomplete at a Missing Expression**

Here's another one. This `my-let` on the last line is missing its body because we're about to write it. Normally that's a syntax error and Racket gives up. But if I ask for autocomplete right there where the body's going to go, it not only works but even includes `y`, which is part of the bad use.

**▶ Demo: Services Inside a Bad Macro Use**

We can take this even further. The outer `my-let` here has a malformed binding group, so this is a bad macro use. Even a language like Rust would give up here. But somehow, we get services inside of the bad macro use. And since the body of the the inner `my-let` is missing like the previous example, it's actually a bad macro use inside of a bad macro use. This is possible thanks to our ~var expr annotation in the macro. This tells treason that the body of a `my-let` is just an ordinary expression, not something the macro specially interprets. So even though the outer use is broken, we can still dive into subexpressions to get services on them.

**▶ Demo: Services in a Template**

One more nice little thing is that we get services inside the template of a macro definition. Here I'm running autocomplete in the template of the macro. Autocomplete suggests the pattern variable `p`, which you'd expect, but it also has `x`, which is a binding introduced by the macro template itself. So our IDE services understand the pattern variables that are available, and the structure of the code the template generates. And again, all of this happens even in an empty `let` body in the template.

Alright, now how does this all work?

**▶ Where Do the Services Come From?**

First, let's take a step back and think about how IDE services work in the first place.

Most IDE services are about variables. Where is this variable defined? Where is this variable used? What variables can I use here? All of these questions are answered during expansion. Let's run through an example.

Here we have a simple program that references a locally defined variable. Let's focus on the reference on line 2.

The expander will know what's in scope at the site of the reference and resolve the reference to the definition on line 1.

This gives us everything we need to know for IDE services! We just record all the definitions, resolutions, and what was in scope as we expand the program and give that information to the IDE.

For go to definition, we just look up the resolution in the table and jump to the binding site. For autocomplete, we pretty much just do a lookup in the table of names that were in scope.

Macro hygiene and fault tolerance makes this all a little more complicated, but that's the idea.

**▶ So Why Does Racket Struggle?**

Now we can see why Racket struggles. Racket's IDE services only look at the end result of expansion and the expander doesn't record this resolution information as it goes in any way that's surfaced to the IDE. So if expansion stops from any error, we get no information to inform services on any part of the file. Not even parts before the error.

So how does Treason get around this? The core idea is that the expander records every variable definition and resolution as it goes, so even if expansion fails we still surface that information to the IDE. And even if there are errors, we just keep expanding so we analyze as much of the program as we can.

**▶ Fault-Tolerant Expansion**

For example, here's a little `define1` macro that turns `(define1 x)` into `(define x 1)`. The first use is fine, but the second is missing the variable so it errors. We just replace that use with a sentinel and keep going. This is nothing new. Languages like rust already do it.

**▶ The Hard Part: Inside a Bad Macro Use (code)**

The harder part is getting services inside of a macro use. Here, we have a `my-define` macro which desugars a function definition into a single-variable definition with a lambda. We have a bad use since it's missing the function name. Normally in Racket, and even in Rust, this would just be a syntax error and we'd get no services on the body. But come on, it's obviously just a bunch of normal expressions! We even annotate them as expressions with ~var expr!

In Treason we leverage these annotations to get services in the body. When we have a bad macro use, we keep matching the syntax against the pattern to try to find the body and we expand those subexpressions in isolation just to get services on them. I call this spec-driven subexpression expansion, and it's the main novel contribution of Treason.

**▶ Cursor-Driven Autocomplete (empty)**

Another nice thing Treason has is autocomplete inside of macro uses, even when they're not done being written. Here's how it works.

We're in this let, and we're about to write the body and we want autocomplete. We start by inserting a bogus identifier where the cursor was, and expand the program. Eventually, we try to resolve the cursor. Of course, it's not bound to anything so this is going to be an error, which is fine. The important part is that in Treason, we record what was in scope when we try to resolve a reference. So we just check what was in scope when the expander tried to resolve the cursor reference and that's what we show in autocomplete. We're kind of flipping binding resolution on its head. If we were in the middle of typing a name, then we'd just use that unfinished identifier as the cursor identifier instead of inserting a bogus one.

And again, hygiene makes this a little more complicated, but that's the main idea.

**▶ Services in Templates (setup)**

One last nice thing treason gives us is services in macro templates. Here we see that `p` is in scope, which is expected since it's a pattern variable, and `m` from the definition site, but also `x` which is bound in the macro-introduced code. We actually get this pretty much for free since we track resolutions as we expand.

Again, since this the service we're using is autocomplete, we insert a bogus cursor identifier. When the expander is going through the definition of the macro, we'll try to resolve the cursor identifier in the template just to see if it resolves to a pattern variable. and it doesn't, so we know it's a macro-introduced identifier in the template. But like any other resolution, we keep track of what was in scope. Then we end up expanding the use, and in there we end up expanding the macro-introduced cursor identifier once again. When resolving this macro-introduced cursor identifier, the macro-introduced binding `x` is in scope, and also `m` from the definition site. Now we have 2 resolutions of the same identifier, which is something that happens when macros are involved. When this happens, autocomplete gives us the union of names in scope from all the resolutions.

One nice thing is that this didn't really have to be baked into treason. By just recording information from resolutions as we expand, we naturally get services in templates from the expansion of macro uses.

One limitation of this is that it only works when your macro has a use. But if we're good htdp students so we're writing uses of our macros before we implement, right everybody?

SSE also has some limitations.

**▶ Incomplete Context**

One is that we expand subexpressions in the context of the use, which is not necessarily the correct context for that subexpression. It may reference bindings internal to the macro, it may depend on syntax parameters established by the macro, stuff like that. But I'd argue it's better to have some possibly incorrect services on bad macro uses rather than nothing. And some of this could be alleviated by declaring binding rules in your macros. More on that later.

**▶ Recursive Macros**

Another limitation is that SSE is a little tricky with recursive macros. Here we have cond, which is implemented recursively. If we have a misuse on the first clause, we get SSE on the first condition, but none on the second clause because SSE never sees what the second clause is supposed to be.

If instead of just saying clause ..., we added more annotations, we can get SSE on the second clause too.

This makes the pattern a little clunky here, but we could use syntax classes to clean it up. And by the way, this is also a limitation of Racket's syntax/parse where the annotations are necessary to get better error messages.

One thing to note is that even with a missing expression, we can still get SSE in that first clause since the condition was there. But things don't always work out so nicely.

**▶ SSE Can Misalign**

SSE can get misaligned. This macro wants a 1, a 2, and then an expression. but if we forget the 2, treason thinks that `let` is supposed to be the 2 and says "hey that's not a 2!". In general, treason doesn't try to distinguish the difference between a missing expression, a wrong expression, or an extra expression. It's kind of prefix-oriented like syntax-parse's notion of match progress.

It's not always clear, but treason could try harder to distinguish between these 3 cases, but that's something we'll leave to future work.

**▶ Open Questions & Future Work**

Another thing we need to do in the future is think about procedural macros. Right now treason only has pattern-based macros like the ones we've seen with syntax-rules. There is nothing in principle preventing us from supporting procedural macros, we just haven't implemented them yet. But it's not as simple as adding runtime support. Side effects are tricky with this fault tolerance because if a macro runs some side effects and then fails, we still keep expanding the rest of the program due to fault tolerance, which could break some assumptions of macro authors. We also have to design how we want to expose runtime support for fault-tolerance and SSE for procedural macros.

We can also do fault-tolerant reading. Right now, if you have mismatched parens, expansion doesn't even happen. This isn't a problem if you're in an IDE that auto-closes parens for you, but it'd still be nice if reading was as fault-tolerant as expansion. We're thinking about trying to use indentation to repair programs that fail to read.

And right now, our language server re-expands every time you edit the program, so we could do something like Rust's salsa framework where we incrementally re-expand only the parts of the program that need to be re-expanded on a change. But again, that could get tricky with side effects.

We also want to add support for declaring binding rules like in syntax spec.

**▶ The syntax-spec Vision (PEG)**

For example, let's say we implement our own pattern-matching macro. This isn't just some simple syntactic sugar like other macros we've seen. This is a full-blown DSL with its own grammar and binding rules for patterns.
> show good use

What would happen happen if we had a malformed pattern?
> show malformed example with no highlights. have red boxes around num and rest

We _could_ get some SSE on the clause body since it's an expr, but treason doesn't know what a pattern is, so there's no way for us to get anything like SSE on the pattern itself. And our match macro also has its own binding rules: We want all of the variables in the pattern to be bound in the body. Treason doesn't know about these rules, so SSE would see the pattern variable references in the body as unbound since it'll expand the body out of context.

Here's what an implementation of this match macro would look like

```racket
(define-syntax my-match
  (syntax-rules (cons)
    [(_ target:expr [(cons (~var pa ???) (~var pd ???)) (~var body expr)])
     ...]
    ...))
```

We can have ~var expr on the body, but there is nothing we can put for pa and pd since treason doesn't know what a pattern is.

In order to tell treason what a pattern is and what the binding rules are for patterns, we could use something like Michael Ballantyne's syntax-spec.

With syntax-spec, we can declare the grammar for our pattern matching macro. So a pattern is either a cons, a variable, or a wildcard pattern. And a clause has a pattern or an expr We can also add binding rules to tell the expander that all pattern variables are exported from the pattern and bound in the body.
> existing syntax-spec slide

Now we could rewrite our macro to annotate the c with a clause, and treason will know what that means.

```racket
(define-syntax my-match
  (syntax-rules (cons)
    [(_ target:expr (~var c clause))
     ...]
    ...))
```

With this, if we go back to our bad use, SSE would be able to figure out that num in the body should be bound by the pattern.
> show slide with num resolving but rest still unbound with red box

In general, the more information available to the expander before a macro expands, the better the IDE experience can be.

**▶ Acknowledgements**

Before I go, I want to thank Michael Ballantyne for making Treason with me, helping me develop this talk, and letting me help him make syntax spec. And of course, thank you to the Racket community for putting this event together and having me here.

**▶ Treason (links + QR)**

And if you're interested, Treason is on GitHub and there's a vscode extension for you to play around with it, and I also recently started a YouTube channel where I talk about math and some programming language stuff.
