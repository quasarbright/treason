# Treason talk script

Slide-transition markers (`▶`) mark moving to the next slide. Reveals within a
slide aren't marked.

---

**▶ Title**

Hi, I'm Mike Delmonaco. I'm a software engineer at Amazon Web Services, and I went to Northeastern, where I learned Racket. And these days I do a little bit of programming language research on the side with Michael Ballantyne. Today I want to show you something I've been working on with Michael Ballantyne called Treason. It's a language with a macro system similar to Racket's, but with better IDE support. Even when the program is broken

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

One last nice thing treason gives us is services in macro templates. Here we see that `p` is in scope, which is expected since it's a pattern variable, but also `x` which is bound in the macro-introduced code. We actually get this pretty much for free since we track resolutions as we expand.

Again, since this the service we're using is autocomplete, we insert a bogus cursor identifier. When the expander is going through the definition of the macro, we'll try to resolve the cursor identifier in the template just to see if it resolves to a pattern variable. and it doesn't, so we know it's a macro-introduced identifier in the template. But like any other resolution, we keep track of what was in scope. Then we end up expanding the use, and in there we end up expanding the macro-introduced cursor identifier once again. When resolving this macro-introduced cursor identifier, the macro-introduced binding `x` is in scope. Now we have 2 resolutions of the same identifier, which is something that happens when macros are involved. When this happens, autocomplete gives us the union of names in scope from all the resolutions.

One nice thing is that this didn't really have to be baked into treason. By just recording information from resolutions as we expand, we naturally get services in templates from the expansion of macro uses.

One limitation of this is that it only works when your macro has a use. But if we're good htdp students so we're writing uses of our macros before we implement, right everybody?

SSE also has some limitations.

**▶ Incomplete Context**

One is that we expand subexpressions in the context of the use, which is not necessarily the correct context for that subexpression. It may reference bindings internal to the macro, it may depend on syntax parameters established by the macro, stuff like that. But I'd argue it's better to have some possibly incorrect services on bad macro uses rather than nothing. And some of this could be alleviated by declaring binding rules in your macros. More on that later.

**▶ Multiple Clauses: Which Subexpressions?**

Another limitation is that recursive macros need to be written in such a way that they validate their syntax upfront with patterns as much as possible.

The second rough edge shows up with multiple clauses. Here's a macro with two clauses. The first one wants a `1` and then an expression; the second wants an expression and then a `2`. And I use it with two `let`s. So which subexpressions do we give services on? Both? Just the first? Just the second? What Treason does is borrow the idea of progress from syntax-parse. It figures out which clause got the furthest, going left to right and outside in, and then it uses that clause's annotations to decide which subexpressions to expand. There's also a wrinkle here where recursive macros have to be written so they validate the full syntax on the first expansion for this to behave, though syntax-parse already wants you to write them that way.

**▶ Progress Can Misalign**

But this progress idea can misalign. Here's a macro that wants `1`, then `2`, then an expression. And say I forget the `2`. Now the `let` slides over into the `2` position. Treason reads it as if it's supposed to be the `2`, so it doesn't expand it, and we get no services on it. And the actual expression slot gets treated as missing. Treason doesn't try to tell the difference between a missing subexpression, an extra one, or a wrong one. So for macros that are really positional, a use with something missing or extra can shift everything over and run SSE on the wrong stuff.

**▶ Open Questions & Future Work**

And there's plenty left to figure out. Procedural macros, meaning arbitrary code in your macros, and how to deal with side effects when you're speculatively expanding. Fault-tolerant reading, because right now if you're missing a paren, you're done. Incremental re-expansion, so the language server isn't redoing everything on every keystroke. And integrating syntax-spec, which I keep bringing up.

**▶ The syntax-spec Vision (PEG)**

So let me actually paint that syntax-spec picture, because I think it's the exciting part. Say you've got a little PEG parser DSL. Here I'm defining a parser for addition. It reads something like "1+2" and builds an addition node. And that last part, `(addition l r)`, that's a regular Racket, or Treason, expression, using the variables `l` and `r` that got bound by the parser.

**▶ The syntax-spec Vision (mistake)**

Now what happens when you make a mistake? Here I forgot to say what `l` binds to. I wrote `(bind l)` instead of `(bind l num-expr)`. And here's the problem. Your whole program here is one big macro use for a custom DSL, and that DSL is mostly opaque to the expander. So one error ruins the fault tolerance, because the expander has no idea what the grammar or the binding rules of your DSL forms are. The best it could do is expand `(addition l r)` in a really incomplete context, which isn't worth much.

**▶ The syntax-spec Vision (declare the rules)**

But if we let you declare the grammar and the binding rules for your DSL forms, then we can bring all of this, the fault tolerance and SSE, into your DSL. In this example, we'd know that `bind` binds a variable and makes it available in the body of the `=>`. So we'd know `l` and `r` are bound in `(addition l r)`, and we could give you real services on it, even with the mistake. Expand what you can, even in a broken DSL use. So that's the vision: Treason plus syntax-spec, good tooling for your own DSLs, not just the core language.

**▶ That's it!**

And that's it.

**▶ Acknowledgements**

I want to thank Michael Ballantyne, who I work with on syntax-spec and who's helped a ton with this. And the Racket community, for making all the tools I get to build on.

**▶ Treason (links + QR)**

And if you're interested, the language and the language server are up on GitHub, and I've got a blog where I write about mostly Racket stuff. There's a QR code up here for it. Thanks.
