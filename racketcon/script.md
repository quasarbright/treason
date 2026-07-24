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

Surely, this is unavoidable. You have opaque macros and you can extend the language however you want, so of course the IDE can't know as much about your programs. You can't have a language as expressive as Racket AND also get good IDE services. Right?

Actually, you can. 

**▶ Other Languages Keep Going (Rust)**

Other languages with macros like Rust already have better IDE services than Racket. The Rust compiler doesn't stop at the first error. It keeps going, and you still get services on the code after the error. And despite the fact that both of these definitions are broken since they reference something unbound, Rust still gives you autocomplete with the broken `bad` in the suggestions. Pretty nice! Let's see what happens when we throw macros in the mix.

**Macro example**

Here is an example of a macro in rust that allows you to write a json map with nice syntax. Let's zoom in on the use. We have an arrow in between each field name and value. If we write those arrows wrong, we get a syntax error. But only the first error! What happened? I thought rust never gave up?

**▶ But Not Rust Macros (why)**

What's going on here is, plain Rust works well because the compiler knows Rust's grammar and its static semantics, so when something goes a little wrong, it can figure out what you probably meant since plain Rust is totally baked in. But the compiler doesn't know as much about your macros since they're opaque syntax to syntax transformations. And if the macro itself isn't written to be fault-tolerant, there's no way for the compiler to make it fault-tolerant for you, which is why we only see the first error in a bad use. Just like Racket. This lack of fault tolerance is all over Racket because EVERYTHING is a macro! That's part of why the services aren't as good.

**▶ The fix**

So the fix has three parts. The first part is to accumulate static information as we expand so even if expansion fails, we can still get partial services. We also want to just keep expanding after errors so we get information on as much of the file as possible, which is easy. To do better than Rust, we'll have to somehow extend this fault tolerance to within a bad macro use, which is the hard part. That's the main novel contribution of Treason, and we'll do it by giving the expander more knowledge about the internal grammar of a macro. Speaking of which, let's see Treason!

**▶ What is Treason?**

Like I said before, Treason is a Racket-like language with better IDE support. Here's an example: We have an unbound variable in the right-hand-side of two definitions. In Racket, we only saw the first error and we got no services on the whole file. But in Treason, we see both errors, we get services like autocomplete, and autocomplete even includes variables whose definitions failed to expand!

**▶ Demo: Autocomplete at a Missing Expression**

Here's another one. This `my-let` on the last line is missing its body because we're about to write it. Normally that's a syntax error and Racket gives up. But if I ask for autocomplete right there where the body's going to go, it not only works but even includes `y`, which is part of the bad use.

**▶ Demo: Services Inside a Bad Macro Use**

We can take this even further. The outer `my-let` here has a malformed binding group, so this is a bad macro use. Even a language like Rust would give up here. But somehow, we get services inside of the bad macro use. And since the body of the the inner `my-let` is missing like the previous example, it's actually a bad macro use inside of a bad macro use. This is possible thanks to our ~var expr annotation in the macro. This tells treason that the body of a `my-let` is just an ordinary expression, not something the macro specially interprets. So even though the outer use is broken, we can still dive into subexpressions to get services on them.

**▶ Demo: Services in a Template**

One more nice little thing is that we get services inside the template of a macro definition. Here I'm running autocomplete in the template of the macro. Autocomplete suggests the pattern variables `m` and `p`, which you'd expect, but it also has `x`, which is a binding introduced by the macro template itself. So our IDE services understand the pattern variables that are available, and the structure of the code the template generates. And again, all of this happens even in an empty `let` body in the template.

Alright, now how is this all possible?

**▶ Where Do the Services Come From?**

First, let's take a step back and think about how IDE services work in the first place.

Most IDE services are about variables. Where is this variable defined? Where is this variable used? What variables can I use here? All of these questions are answered during expansion. Let's run through an example.

Here we have a simple program that references a locally defined variable. Let's focus on the reference on line 2.

The expander will know what's in scope at the site of the reference and resolve the reference to the definition on line 1.

This gives us everything we need to know for IDE services! We just record all the definitions, resolutions, and what was in scope as we expand the program and give that information to the IDE.

For go to definition, we just look up the resolution in the table and jump to the binding site. For autocomplete, we pretty much just do a lookup in the table of names that were in scope.

Macro hygiene and fault tolerance makes this all a little more complicated, but that's the idea.

**▶ So Why Does Racket Struggle?**

Now we can see why Racket struggles. Racket's IDE services only look at the end result of expansion and the expander doesn't record this resolution information as it goes in any way that's surfaced to the IDE. So if expansion stops from any error, we don't get the expansion result, so we don't get any information, which means no services on any part of the file. Not even parts before the error.

So how does Treason get around this? The core idea is that the expander records every variable definition and resolution as it goes, so even if expansion fails we still surface that information to the IDE. And even if there are errors, we just keep expanding so we analyze as much of the program as we can.

**▶ Fault-Tolerant Expansion**

For example, here's a tiny `define1` macro that turns `(define1 x)` into `(define x 1)`. The expander walks the forms one at a time. First, `(define1 x)` expands to `(define x 1)`, no problem. Next it hits `(define1)`, a bad use, missing its argument. Instead of giving up, the expander replaces that whole form with what I call a sentinel node, something that says "there was an error here," and keeps going. So it still reaches `(define1 y)` and expands it to `(define y 1)`. The error in the middle doesn't stop us from getting to `y`. This part isn't novel. Rust does it, Lean does it. But even this on its own is a huge win and gives us way more services in bad programs.

**▶ The Hard Part: Inside a Bad Macro Use (code)**

But the harder part, the part that languages like Rust don't solve, is getting services inside a bad macro use. So look at this. I've got a `my-define` macro. It takes a function header and a body, and expands into a single-variable `define` with a `lambda`. And down here I use it, but I've left the function header empty. Just `()`. No function name, no parameters.

Languages like Rust would just give up here and move on to the next expression after this bad macro use. But it would be nice if we could somehow get services in the body of that definition. A little mistake shouldn't ruin all that nice code in there!

This is where the main novel contribution of treason comes in. Something I call spec-driven subexpression expansion, or SSE for short

**▶ Spec-Driven Subexpression Expansion**

Look at the definition: the body patter variable is annotated `expr` and that corresponds to the square root expression in the use. It's just a plan old treason expression, not something specially interpreted by the macro. So even though it's a bad use and we can't actually instantiate the template since some parts are missing, we can still match up the body pattern variable to that square root, realize it's an expression from the annotation, and then expand that on its own just to get the analysis on it.

That's how SSE works. We leverage annotations to learn a little more about the macro's intended grammar.

**▶ Cursor-Driven Autocomplete (empty)**

One more mechanism, and I'll want it for the next part. How do we do autocomplete, especially when the spot you're completing at is empty? Here the body of this `let` is empty, and the blue bar is just where the user's cursor is. There's nothing there to complete yet.

**▶ Cursor-Driven Autocomplete (insert)**

So we do something a little sneaky. We insert a cursor identifier, `_cursor1234`, right where the user's cursor is, and we expand the program.

**▶ Cursor-Driven Autocomplete (reach)**

Expanding, we reach the cursor.

**▶ Cursor-Driven Autocomplete (resolve)**

Now, the cursor identifier is made up, it's not bound to anything, so it resolves to nothing, an unbound error. And that's totally expected and fine, we put it there on purpose.

**▶ Cursor-Driven Autocomplete (answer)**

What we actually care about is the other column: what's in scope at the cursor. Here, just `x`. And that in-scope set is exactly the autocomplete list. If we didn't have macros, we could just read off what's in scope at the surrounding form. But with hygienic macros, working out what's really in scope is the sneaky part, and getting it right is what this buys us.

**▶ Services in Templates (setup)**

Now here's a nice payoff of all this. We get services inside the template of a macro definition. Let me walk through how. Here's a macro `m`, and I've inserted a cursor identifier, `_cursor1234`, right in the body of the template's `let`. And down here there's a use, `(m 1)`. Watch the resolution table fill in as we expand.

**▶ Services in Templates (reference)**

First we expand the definition. Now, the template on line 3 has no binding structure yet. It's just syntax. So even though `x` looks like it's in a binding position there in the `let`, we treat it as a reference.

**▶ Services in Templates (resolve)**

And when we resolve a template variable, it either resolves to a pattern variable, which will get replaced by use-site syntax, or to nothing, meaning it's macro-introduced. `x` isn't a pattern variable, so it resolves to nothing. And the pattern variables `m` and `p` are in scope. That's our first table entry.

**▶ Services in Templates (use)**

Now we move on to the use down here, `(m 1)`.

**▶ Services in Templates (replace)**

We expand it, so `(m 1)` gets replaced by the template body. The pattern variable `p` gets filled in with the use-site `1`, so it becomes `(let ([x 1]) _cursor1234)`.

**▶ Services in Templates (cursor)**

And we walk that expanded output just like any program. We reach the cursor, the one in the expanded use. This time `x` really is in scope, because the `let` binds it.

**▶ Services in Templates (resolve cursor)**

So the cursor resolves to the `x` on line 3, and `x` is in scope. That's our second entry. Notice only `x` is in scope here, not the pattern variables.

**▶ Services in Templates (payoff)**

And that's why autocomplete in the template works. `m` and `p` are pattern variables, we always had those. The interesting one is `x`, the macro-introduced binding, and we only learned about it by expanding a use. That's the tradeoff: it needs a use to exist somewhere. No uses, no template services. We could get around that by eagerly expanding templates, but that would restrict what your macros are allowed to do, so we require a use. And if you're a good HtDP student, you're writing your uses before you implement the macro anyway.

**▶ Limitations**

So, SSE is powerful, but it has some rough edges. There are three big ones. SSE expands under an incomplete context, recursive macros need to be written a certain way, and missing or extra subexpressions can misalign SSE. Let me walk through those.

**▶ Incomplete Context**

Back to that `my-define` use. In the body, `x` and `y` don't resolve, and those were supposed to be the parameters. Because the pattern failed, no binding for `x` or `y` was ever made, so we never find out they're supposed to be in scope. We're expanding the body on its own, under the whole macro use, so bindings from inside the use, syntax parameters, that kind of thing, might be missing. And that could get weird if there are side effects. Now, if we could declare the binding rules of the macro up front, say that `x` and `y` are bound in the body, then we'd get them too, even in a broken use. And that's exactly the kind of thing syntax-spec does, which is the tool I work on with Michael. So that's a direction we could take Treason.

**▶ Multiple Clauses: Which Subexpressions?**

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
