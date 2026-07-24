#lang slideshow

(require slideshow/code slideshow/text pict/color racket/runtime-path
         simple-qr racket/draw)

(define (vert . picts) (apply vl-append 10 picts))
(define (horiz . picts) (apply hc-append 5 picts))

;; images live next to this file; load them by absolute path so the deck
;; works from any directory
(define-runtime-path here ".")
(define (img name [w 800] [h 420])
  (scale-to-fit (bitmap (build-path here name)) w h))

(define-syntax-rule (framed-code datum ...)
  (frame (code datum ...) #:color "red" #:line-width 3))

;; a template position that can't be filled
(define hole (colorize (tt "?") "red"))

;; codep : inline code glued to trailing punctuation (no space before it)
(define-syntax-rule (codep datum punc)
  (hbl-append (code datum) (t punc)))

;; red-squiggle : Real [Real] -> pict
;; a wavy red underline, like an editor's error highlight
(define (red-squiggle w [h 5])
  (dc (lambda (dc dx dy)
        (define old-pen (send dc get-pen))
        (send dc set-pen (make-pen #:color "red" #:width 1.5))
        (let loop ([x 0] [up? #t])
          (when (< x w)
            (define nx (min w (+ x 4)))
            (send dc draw-line (+ dx x) (+ dy (if up? h 0))
                               (+ dx nx) (+ dy (if up? 0 h)))
            (loop nx (not up?))))
        (send dc set-pen old-pen))
      w h))

;; error-box : String -> pict
;; a diagnostic message styled like an editor error tooltip
(define (error-box msg)
  (frame (inset (hc-append 8 (colorize (bt "✕") "red")
                           (with-size 22 (t msg)))
                10)
         #:color "red" #:line-width 2))

(define (highlight p #:color [color "yellow"] #:padding [padding 10])
  (cc-superimpose
   (inset (filled-rectangle (+ padding (pict-width p)) (+ padding (pict-height p))
                            #:color color #:draw-border? #f)
          (- (+ padding 10)))
   p))

(slide
 (titlet "Treason: Making Macros and IDE Services Work Together")
 (titlet "Mike Delmonaco"))

(slide
 #:title "Agenda"
 (item "Racket's poor IDE experience")
 (item "Some of these problems have been solved in other languages")
 (item "Treason in action: Even better than those other languages thanks to a new technique!")
 (item "How Treason works")
 (item "Limitations and future work"))

;; ---------------------------------------------------------------------------
;; The problem
;; ---------------------------------------------------------------------------

(slide
 #:title "Racket's Poor IDE Experience"
 (img "racket-no-auto.png")
 (item "Services not available when there is an error (which is most of the time)")
 (item "Only shows the first error"))

(define mag-last (code (define mag-sq (+ x2 y2))))
(define mag-code
  (code
   (code:comment "Real Real -> Real")
   (define (magnitude x y)
     (define x2 (sqr x))
     (define y2 (sqr y))
     #,mag-last)))
(define mag-code/error
  (pin-under mag-code
             mag-last lb-find
             (red-squiggle (pict-width mag-last))))

(slide
 #:title "You Have Errors Most of the Time"
 #:layout 'top
 (t "An unfinished definition body")
 (blank 15)
 mag-code/error
 (blank 15)
 (error-box "the last form is not an expression")
 (blank 20)
 (para #:align 'center "We still need to add the final expression, " (codep (sqrt mag-sq) ".")))

(define stud-ag (code average-grade))
(define stud-gl (code grade->letter))
(define stud-code
  (code
   (define (student->report s)
     (define avg (#,stud-ag (student-grades s)))
     (report (student-name s)
             avg
             (#,stud-gl avg)))))
(define stud-code/error
  (pin-under stud-code stud-ag lb-find (red-squiggle (pict-width stud-ag))))

(slide
 #:title "You Have Errors Most of the Time"
 #:layout 'top
 (t "Top-down style")
 (blank 15)
 stud-code/error
 (blank 15)
 (error-box "average-grade: unbound identifier")
 (blank 20)
 (para #:align 'center (code average-grade) "and" (code grade->letter)
       "aren't defined yet."))

;; ---------------------------------------------------------------------------
;; The provocation
;; ---------------------------------------------------------------------------

(slide
 #:title "A Tradeoff?"
 (t "This is just the price of macros")
 (blank 20)
 (t "You can't have this much expressiveness AND good IDE services"))

;; ---------------------------------------------------------------------------
;; Other languages
;; ---------------------------------------------------------------------------

(slide
 #:title "Other Languages Keep Going"
 (t "Rust has macros, and doesn't quit on the first error.")
 (t "You even get autocomplete on the broken definitions.")
 (img "Pasted image 20260610201130.png" 760 360))

(slide
 #:title "Other Languages Keep Going"
 (img "rust-macro-good.png" 600 470))

(slide
 #:title "Other Languages Keep Going"
 (img "rust-macro-good-use.png" 800 550)
 (para #:align 'center "Uses" (code =>) "as a separator"))

(slide
 #:title "Other Languages Keep Going"
 (img "rust-macro-bad-use.png" 700 550)
 (t "Only the first error inside of a bad use"))

(slide
 #:title "Other Languages Keep Going"
 (para "Error recovery in plain Rust works because the compiler knows the grammar and can guess what you meant.")
 (blank 15)
 (item "Macros are opaque syntax-to-syntax transformations")
 (item "Macros are not built to be fault-tolerant")
 (item "But in Racket, everything is a macro!"))

(slide
 #:title "The Fix"
 (item "Accumulate static information as you expand")
 (item "Keep going after errors")
 (item "Retain services inside a bad macro use by making macros less opaque"))

;; ---------------------------------------------------------------------------
;; Treason: what it looks like
;; ---------------------------------------------------------------------------

(slide
 #:title "Treason"
 (t "A Racket-like language with better IDE support.")
 (img "treason-yes-auto.png" 700 300)
 (item "We get multiple errors")
 (item "Autocomplete works despite errors, and even includes ill-defined variables"))

(slide
 #:title "Autocomplete at a Missing Expression"
 (img "Pasted image 20260529105419.png" 820 320)
 (para #:align 'center "The" (code my-let)
       "body is missing, but autocomplete works and even includes" (code y)))

(slide
 #:title "Demo: Services Inside a Bad Macro Use"
 (img "Pasted image 20260529102934.png" 820 300)
 (para "The outer" (code my-let)
       "is malformed, but we get services in the inner one")
 (para "Powered by the" (code (~var b expr)) "annotation")
 (para "Other languages don't do this!"))

(slide
 #:title "Demo: Services in a Template"
 (img "service-in-template.png" 820 280)
 (para #:align 'center "Autocomplete has pattern vars" (code m) "and"
       (codep p ",") "plus the macro-introduced" (codep x ",")
       "even in an empty" (code let) "body."))

;; ---------------------------------------------------------------------------
;; How expansion gives rise to IDE services
;; ---------------------------------------------------------------------------

;; annotate-box : scene target -> scene
;; outline box over target, drawn without changing the scene's layout
(define (annotate-box scene target #:color [c "orange"] #:pad [pad 3])
  (define-values (x y) (lt-find scene target))
  (pin-over scene (- x pad) (- y pad)
            (frame (blank (+ (pict-width target) (* 2 pad))
                          (+ (pict-height target) (* 2 pad)))
                   #:color c #:line-width 3)))

;; annotate-fill : scene (or target (listof target)) -> scene
;; color fill behind each target, drawn without changing the scene's layout
(define (annotate-fill scene target-or-targets #:color [c (light (light "blue"))] #:pad [pad 2])
  (for/fold ([scene scene])
            ([target (if (list? target-or-targets) target-or-targets (list target-or-targets))])
    (define-values (x y) (lt-find scene target))
    (pin-under scene (- x pad) (- y pad)
               (filled-rectangle (+ (pict-width target) (* 2 pad))
                                 (+ (pict-height target) (* 2 pad))
                                 #:color c #:draw-border? #f))))

;; the walkthrough program, with the two x occurrences as grabbable sub-picts
;; so we can box / highlight them (no arrows)
(define mech-xdef (code x))
(define mech-xuse (code x))
(define mech-prog
  (code
   (define #,mech-xdef 1)
   (define y (+ #,mech-xuse 2))))
;; the reference boxed (we're expanding it)
(define mech-prog/box (annotate-box mech-prog mech-xuse))
;; reference and its binding filled the same color (the resolution)
(define mech-prog/resolved
  (annotate-box
   (annotate-fill (annotate-fill mech-prog mech-xdef) mech-xuse)
   mech-xuse))
(define mech-table
  (table 3
         (list (bt "reference")                       (bt "what it resolved to")             (bt "what was in scope")
               (hbl-append (code x) (t " on line 2")) (hbl-append (code x) (t " on line 1")) (hbl-append (code x) (t ", ") (code y)))
         lc-superimpose cc-superimpose 40 10))

(slide
 #:title "Where Do the Services Come From?"
 #:layout 'top
 (para "Most IDE services are focused on variables")
 (item "Go to definition")
 (item "Find references")
 (item "Autocomplete")
 (para "Expansion captures this information"))

(slide
 #:title "Where Do the Services Come From?"
 #:layout 'top
 (para #:align 'center "As the expander walks the program, it works out what each name refers to.")
 (blank 30)
 mech-prog)

(slide
 #:title "Where Do the Services Come From?"
 #:layout 'top
 (para #:align 'center "Expansion reaches the reference" (codep x ".") "Right now,"
       (code x) "and" (code y) "are in scope.")
 (blank 30)
 mech-prog/box)

(slide
 #:title "Where Do the Services Come From?"
 #:layout 'top
 (para #:align 'center (code x) "resolves to the binding above.")
 (blank 30)
 mech-prog/resolved)

(slide
 #:title "Where Do the Services Come From?"
 #:layout 'top
 (para #:align 'center "We record the resolution, and what was in scope for it.")
 (blank 30)
 mech-prog/resolved
 (blank 30)
 mech-table)

(slide
 #:title "Where Do the Services Come From?"
 (item "Go-to-definition: look up the resolution, jump to the binding")
 (item "Autocomplete: look up what was in scope, offer those names"))

(slide
 #:title "Racket vs Treason"
 (item "Racket's IDE services operate on the fully expanded program: All or nothing")
 (item "Treason records variable resolutions as it expands")
 (item "Treason does not stop expanding on errors"))

;; ---------------------------------------------------------------------------
;; Fault tolerance
;; ---------------------------------------------------------------------------

;; a sentinel node: what a bad piece of syntax gets replaced with
(define sentinel
  (cc-superimpose
   (filled-rectangle 62 30 #:color "red" #:draw-border? #f)
   (colorize (tt "ERR") "white")))

;; expansion walkthrough: expand each form in turn, boxing the next one.
;; (define1 x) => (define x 1); the bad use (define1) becomes a sentinel.
(define ft-form1 (code (define1 x)))
(define ft-form2 (code (define1)))
(define ft-form3 (code (define1 y)))

(define ft-progA
  (code
   (define-syntax define1
     (syntax-rules ()
       [(define1 x) (define x 1)]))
   code:blank
   #,ft-form1
   (define1)
   (define1 y)))
(define ft-progB
  (code
   (define-syntax define1
     (syntax-rules ()
       [(define1 x) (define x 1)]))
   code:blank
   (define x 1)
   #,ft-form2
   (define1 y)))
(define ft-progC
  (code
   (define-syntax define1
     (syntax-rules ()
       [(define1 x) (define x 1)]))
   code:blank
   (define x 1)
   #,sentinel
   #,ft-form3))
(define ft-progD
  (code
   (define-syntax define1
     (syntax-rules ()
       [(define1 x) (define x 1)]))
   code:blank
   (define x 1)
   #,sentinel
   (define y 1)))

(slide #:title "Fault-Tolerant Expansion" #:layout 'top
       (annotate-box ft-progA ft-form1))
(slide #:title "Fault-Tolerant Expansion" #:layout 'top
       (annotate-box ft-progB ft-form2))
(slide #:title "Fault-Tolerant Expansion" #:layout 'top
       (annotate-box ft-progC ft-form3))
(slide #:title "Fault-Tolerant Expansion" #:layout 'top
       ft-progD)

;; ---------------------------------------------------------------------------
;; SSE
;; ---------------------------------------------------------------------------

(slide
 #:title "The Hard Part: Inside a Bad Macro Use"
 #:layout 'top
 (code
  (define-syntax my-define
    (syntax-rules ()
      [(my-define (f x ...) (~var body expr) ...)
       (define f (lambda (x ...) (block body ...)))]))
  code:blank
  (my-define #,(framed-code ())
    (sqrt (+ (sqr x) (sqr y)))))
 (t "The header is empty: no function name, no parameters."))

;; SSE: link the (~var body expr) annotation in the definition to the
;; matching subexpression in the use, both filled the same color
(define sse-annot (code (~var body expr)))
(define sse-body (code (sqrt (+ (sqr x) (sqr y)))))
(define sse-code
  (annotate-fill
   (code
    (define-syntax my-define
      (syntax-rules ()
        [(my-define (f x ...) #,sse-annot ...)
         (define f (lambda (x ...) (block body ...)))]))
    code:blank
    (my-define ()
      #,sse-body))
   (list sse-annot sse-body) #:color (light (light "green"))))

(slide
 #:title "Spec-Driven Subexpression Expansion"
 #:layout 'top
 sse-code
 (blank 20)
 (para #:align 'center "The annotation says the body is an" (codep expr ",")
       "so we expand that subexpression on its own."))

;; cap : fixed-height caption area, so the code below never shifts between steps
(define CAP-W 1000)
(define CAP-H 130)
(define (cap . parts)
  (ct-superimpose
   (blank CAP-W CAP-H)
   (parameterize ([current-para-width CAP-W])
     (apply para #:align 'center parts))))

;; ---------------------------------------------------------------------------
;; Cursor-driven autocomplete (established before templates): step-by-step
;; ---------------------------------------------------------------------------

(slide #:title "Cursor-Driven Autocomplete"
  (img "autocomplete-no-body.png")
  (t "How does autocomplete get x?"))

;; a text caret marking the user's cursor in an otherwise empty body
(define caret (filled-rectangle 3 (pict-height (code x)) #:color "royalblue"))
(define cur-x (code x))
(define cur-cursor (code _cursor1234))
(define cur-prog-empty (code (let ([x 1]) #,caret)))
(define cur-prog (code (let ([#,cur-x 1]) #,cur-cursor)))
(define cur-prog/resolved
  (annotate-box (annotate-fill cur-prog cur-x #:color (light (light "green")))
                cur-cursor))
(define cur-headers (list (bt "reference") (bt "resolves to") (bt "in scope")))
(define cur-row (list (code _cursor1234) (t "nothing (unbound)") (code x)))
(define (cur-table . rows)
  (table 3 (append cur-headers (apply append rows))
         lc-superimpose cc-superimpose 40 10))
(define (cur-slide caption program table)
  (slide #:title "Cursor-Driven Autocomplete" #:layout 'top
         caption (blank 20) program (blank 30) table))

(cur-slide
 (cap "The body is empty. The blue bar is where the user's cursor is."
      "Nothing there to complete yet.")
 cur-prog-empty
 (cur-table))

(cur-slide
 (cap "So we insert a cursor identifier," (codep _cursor1234 ",")
      "right there, and expand.")
 cur-prog
 (cur-table))

(cur-slide
 (cap "Expanding, we reach the cursor.")
 (annotate-box cur-prog cur-cursor)
 (cur-table))

(cur-slide
 (cap "The cursor identifier is made up, bound to nothing, so it resolves to"
      "nothing: an unbound error. That's expected and fine.")
 cur-prog/resolved
 (cur-table cur-row))

(cur-slide
 (cap "What we care about is what's in scope there:" (codep x ".")
      "That's the autocomplete list.")
 cur-prog/resolved
 (cur-table cur-row))

;; ---------------------------------------------------------------------------
;; Services in templates: step-by-step, growing the resolution table
;; ---------------------------------------------------------------------------

;; two program states, same line count so the layout never shifts:
;; A shows the use (m y); B shows it replaced by the instantiated template.
(define tmpl-x (code x))
(define tmpl-call (code (m 1)))
(define tmpl-prog-A
  (code
   (define-syntax m
     (syntax-rules ()
       [(m p) (let ([#,tmpl-x p]) _cursor1234)]))
   code:blank
   #,tmpl-call))
(define tmpl-use-cur (code _cursor1234))
(define tmpl-use-expansion (code (let ([x 1]) #,tmpl-use-cur)))
(define tmpl-prog-B
  (code
   (define-syntax m
     (syntax-rules ()
       [(m p) (let ([x p]) _cursor1234)]))
   code:blank
   #,tmpl-use-expansion))

(slide #:title "Services in Templates" #:layout 'top
  (img "service-in-template.png")
  (t "How does autocomplete get x?"))

;; ln : pict -> pict   tag a table cell with its source line
(define (ln p) (hbl-append p (t " (line 3)")))
(define tmpl-headers (list (bt "reference") (bt "resolves to") (bt "in scope")))
(define tmpl-row1
  (list (ln (code x)) (t "nothing (macro-introduced)")
        (hbl-append (code m) (t ", ") (code p))))
(define tmpl-row2
  (list (ln (code _cursor1234)) (ln (code x)) (ln (code x))))
(define (tmpl-table . rows)
  (table 3 (append tmpl-headers (apply append rows))
         lc-superimpose cc-superimpose 40 10))

;; tmpl-slide : caption program table -> slide, with a fixed layout so only
;; the boxes and the table's rows change between steps
(define (tmpl-slide caption program table)
  (slide #:title "Services in Templates" #:layout 'top
         caption
         program
         (blank 30)
         table))

(tmpl-slide
 (cap "Insert a cursor" (code _cursor1234) "in the template body, then"
      "expand. Watch the table fill in.")
 tmpl-prog-A
 (tmpl-table))

(tmpl-slide
 (cap "First we expand the definition. The template has no binding structure"
      "yet, so" (code x) "is treated as a reference.")
 (annotate-box tmpl-prog-A tmpl-x)
 (tmpl-table))

(tmpl-slide
 (cap "A template variable resolves to a pattern variable, or to nothing if"
      "it's macro-introduced." (code x) "is macro-introduced, so: nothing.")
 (annotate-box tmpl-prog-A tmpl-x)
 (tmpl-table tmpl-row1))

(tmpl-slide
 (cap "Now we move on to the use.")
 (annotate-box tmpl-prog-A tmpl-call)
 (tmpl-table tmpl-row1))

(tmpl-slide
 (cap "Expanding it," (code (m 1)) "is replaced by the template body, with"
      (code p) "filled in as" (codep 1 "."))
 (annotate-box tmpl-prog-B tmpl-use-expansion)
 (tmpl-table tmpl-row1))

(tmpl-slide
 (cap "We reach the cursor in the expanded use. Now" (code x) "is in scope.")
 (annotate-box tmpl-prog-B tmpl-use-cur)
 (tmpl-table tmpl-row1))

(tmpl-slide
 (cap "The cursor resolves to the" (code x) "on line 3.")
 (annotate-box tmpl-prog-B tmpl-use-cur)
 (tmpl-table tmpl-row1 tmpl-row2))

;; ---------------------------------------------------------------------------
;; Limitations
;; ---------------------------------------------------------------------------

(slide
 #:title "Limitations"
 (item "Incomplete context: SSE expands under the whole use,"
       "so bindings and syntax parameters may be missing")
 (item "Recursive macros must validate up front, the style"
       (code syntax-parse) "already wants")
 (item "Missing / extra subexpressions can misalign SSE"))

(slide
 #:title "Incomplete Context"
 (para #:align 'center "Back to the" (code my-define) "use: in the body,"
       (code x) "and" (code y)
       "don't resolve. They were supposed to be the parameters.")
 (item "The pattern failed, so no binding for" (code x) "or" (code y)
       "was ever made")
 (item "We never learn they should be in scope in the body")
 (item "Best we can do: expand the body in the use's context")
 'next
 (blank 15)
 (para #:align 'center "If we could declare binding rules that"
       (codep x ",") (code y) "are bound in the body, we'd get them"
       "too. That's what" (codep syntax-spec ".")))

(slide
 #:title "Multiple Clauses: Which Subexpressions?"
 #:layout 'top
 (code
  (define-syntax m
    (syntax-rules ()
      [(m 1 (~var e expr)) 1]
      [(m (~var e expr) 2) 2]))
  (m (let ([x 3]) x) (let ([y 4]) y)))
 'next
 (t "Both? Just the first? Just the second?")
 'next
 (para #:align 'center (codep syntax-parse "-style")
       "progress: the clause that got furthest wins, and its"
       "annotations pick the subexpressions."))

(slide
 #:title "Progress Can Misalign"
 #:layout 'top
 (code
  (define-syntax m
    (syntax-rules ()
      [(m 1 2 (~var e expr)) 1]))
  (m 1 (let ([y 4]) y)))
 'next
 (para #:align 'center "Forget the" (codep 2 ",") "and the" (code let)
       "is read as the missing" (codep 2 ".") "It isn't expanded, so no"
       "services on it." (code e) "is treated as missing.")
 'next
 (para #:align 'center "Treason doesn't tell missing / extra / wrong"
       "apart, so positional macros can misalign SSE."))

(slide
 #:title "Open Questions & Future Work"
 (item "Procedural macros: how do we handle side effects?")
 (item "Fault-tolerant reading (missing parens are still fatal)")
 (item "Incremental re-expansion on edits")
 (item (code syntax-spec) "integration"))

;; ---------------------------------------------------------------------------
;; syntax-spec vision
;; ---------------------------------------------------------------------------

(slide
 #:title "The syntax-spec Vision"
 #:layout 'top
 (t "A PEG-parser DSL")
 (code
  (struct addition [l r])
  (code:comment "parses \"1+2\" as (addition 1 2)")
  (define-peg add-expr
    (=> (seq (bind l num-expr) "+" (bind r num-expr))
        (addition l r)))))

(slide
 #:title "The syntax-spec Vision"
 #:layout 'top
 (t "Now make a mistake")
 (code
  (struct addition [l r])
  (define-peg add-expr
    (=> (seq (bind #,(framed-code l)) "+" (bind r num-expr))
        (addition l r))))
 'next
 (t "One big, opaque macro use. Any error ruins fault-tolerance.")
 (t "The expander doesn't know the DSL's grammar or binding rules."))

(slide
 #:title "The syntax-spec Vision"
 (t "Let users declare grammar and binding rules for DSL forms.")
 'next
 (blank 20)
 (item "We'd know" (code bind) "binds a variable, available in the"
       (code =>) "body")
 (item "So" (code l) "and" (code r) "are bound in" (code (addition l r)))
 (item "And we get real services on it"))

;; ---------------------------------------------------------------------------
;; End
;; ---------------------------------------------------------------------------

(slide
 (titlet "That's it!"))

(slide
 #:title "Acknowledgements"
 (item "Michael Ballantyne")
 (item "The Racket community"))

;; regenerate the blog QR each build; simple-qr isn't a treason dependency,
;; so keep it out of info.rkt and assume it's installed locally
(define blog-qr-path (path->string (build-path here "blog-qr.png")))
(qr-write "https://quasarbright.github.io/blog/" blog-qr-path)
(define blog-qr (scale (bitmap (read-bitmap blog-qr-path)) 0.5))

(slide
 #:title "Treason"
 (t "The language and language server are on GitHub")
 (hyperlinkize (t "https://github.com/quasarbright/treason"))
 (blank 20)
 (t "My (mostly Racket) blog")
 (hyperlinkize (t "https://quasarbright.github.io/blog/"))
 blog-qr)
