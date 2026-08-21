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

;; ANNOT-COLOR : the fill used to highlight a `~var` annotation
(define ANNOT-COLOR (light (light "green")))
;; further fills, for pairing several annotations with the fragments they expand
(define ANNOT-COLOR-2 (light (light (light "blue"))))
(define ANNOT-COLOR-3 (light (light "orange")))
(define ANNOT-COLOR-4 (light (light (light "magenta"))))

;; annotate-pairs : scene (Listof (Pairof color (Listof target))) -> scene
;; Fills each group of targets in its own color, so an annotation and the
;; fragment of the use it expands are visibly the same thing.
(define (annotate-pairs scene color+targets)
  (for/fold ([scene scene]) ([p color+targets])
    (annotate-fill scene (cdr p) #:color (car p))))

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
 (item "Services like autocomplete not available when there is an error (which is most of the time)")
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
 #:title "Rust keeps going"
 (t "Rust has macros, and doesn't quit on the first error.")
 (t "You even get autocomplete on the broken definitions.")
 (img "Pasted image 20260610201130.png" 760 360))

(slide
 #:title "Bad services inside of Rust macros"
 (para #:align 'center (code json_map!) "is a macro we defined")
 (img "rust-macro-good-use.png" 800 550)
 (para #:align 'center "Uses" (code =>) "as a separator")
 )

(slide
 #:title "Bad services inside of Rust macros"
 (img "rust-no-auto-in-macro.png" 800 550)
 (para #:align 'center "No autocomplete in a macro use")
 )

(slide
 #:title "Bad services inside of Rust macros"
 (img "rust-macro-bad-use.png" 800 550)
 (t "Only the first error inside of a bad use"))

(slide
 #:title "Bad services inside of Rust macros"
 (para "Error recovery in plain Rust works because the compiler knows the grammar and can guess what you meant.")
 (blank 15)
 (item "Macros are opaque syntax-to-syntax transformations")
 (item "Macros are not built to be fault-tolerant")
 (item "But in Racket, everything is a macro!"))

(define fix-annot (code (~var body expr)))
(define fix-code
  (annotate-fill
   (code
    (define-syntax my-define
      (syntax-rules ()
        [(my-define (f x ...)
           #,fix-annot ...)
         (define f
           (lambda (x ...)
             (block body ...)))])))
   fix-annot #:color ANNOT-COLOR))

(slide
 #:title "The Fix"
 (t "Retain services inside a bad macro use by making macros less opaque")
 (blank 15)
 fix-code
 (blank 15)
 (t "Leverage annotations!"))

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
 (para #:align 'center "Autocomplete has pattern var"
       (codep p ",") "plus the macro-introduced" (codep x ",")
       "even in an empty" (code let) "body."))

;; ---------------------------------------------------------------------------
;; How expansion gives rise to IDE services
;; ---------------------------------------------------------------------------

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
      [(my-define (f x ...)
         (~var body expr) ...)
       (define f
         (lambda (x ...)
           (block body ...)))]))
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
        [(my-define (f x ...)
           #,sse-annot ...)
         (define f
           (lambda (x ...)
             (block body ...)))]))
    code:blank
    (my-define ()
      #,sse-body))
   (list sse-annot sse-body) #:color ANNOT-COLOR))

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

;; Hygiene colors: where an identifier came from. The point of the section is
;; that the cursor is macro-introduced, so use-site bindings can't reach it.
(define INTRODUCED-COLOR (light (light (light "magenta"))))
(define USE-SITE-COLOR (light (light "cyan")))

;; swatch : color String -> pict   a legend chip
(define (swatch c label)
  (hc-append 8 (frame (filled-rectangle 22 22 #:color c #:draw-border? #f)
                      #:color "gray")
             (t label)))
(define hygiene-legend
  (hc-append 40
             (swatch INTRODUCED-COLOR "introduced by the macro")
             (swatch USE-SITE-COLOR "from the macro use")))

;; two program states, same line count so the layout never shifts:
;; A shows the use (m 2); B shows it replaced by the instantiated template.
(define tmpl-a-tlet (code let))
(define tmpl-a-x (code x))
(define tmpl-a-cur (code _cursor1234))
(define tmpl-a-arg (code 2))
(define tmpl-call (code (m #,tmpl-a-arg)))
(define tmpl-prog-A
  (annotate-pairs
   (code
    (define-syntax m
      (syntax-rules ()
        [(m p) (#,tmpl-a-tlet ([#,tmpl-a-x p]) #,tmpl-a-cur)]))
    code:blank
    (let ([y 1]) #,tmpl-call))
   (list (cons INTRODUCED-COLOR (list tmpl-a-tlet tmpl-a-x tmpl-a-cur))
         (cons USE-SITE-COLOR (list tmpl-a-arg)))))

(define tmpl-b-tlet (code let))
(define tmpl-b-tx (code x))
(define tmpl-b-tcur (code _cursor1234))
(define tmpl-b-elet (code let))
(define tmpl-b-x (code x))
(define tmpl-b-arg (code 2))
(define tmpl-b-cur (code _cursor1234))
(define tmpl-use-expansion
  (code (#,tmpl-b-elet ([#,tmpl-b-x #,tmpl-b-arg]) #,tmpl-b-cur)))
(define tmpl-prog-B
  (annotate-pairs
   (code
    (define-syntax m
      (syntax-rules ()
        [(m p) (#,tmpl-b-tlet ([#,tmpl-b-tx p]) #,tmpl-b-tcur)]))
    code:blank
    (let ([y 1]) #,tmpl-use-expansion))
   (list (cons INTRODUCED-COLOR
               (list tmpl-b-tlet tmpl-b-tx tmpl-b-tcur
                     tmpl-b-elet tmpl-b-x tmpl-b-cur))
         (cons USE-SITE-COLOR (list tmpl-b-arg)))))

(slide #:title "Services in Templates" #:layout 'top
  (img "service-in-template.png")
  (t "How does autocomplete get x, and why not y?"))

;; tint : pict color -> pict   a colored background behind one name.
;; refocus keeps the result's bounding box and baseline those of p, so tinting
;; a cell doesn't shift the text beside it or grow the row.
(define (tint p c)
  (refocus (cc-superimpose
            (filled-rectangle (+ 4 (pict-width p)) (+ 4 (pict-height p))
                              #:color c #:draw-border? #f)
            p)
           p))
;; ln : pict -> pict   tag a table cell with its source line
(define (ln p) (hbl-append p (t " (line 3)")))
(define tmpl-headers (list (bt "reference") (bt "resolves to") (bt "in scope")))
;; The same cursor gets resolved twice: once in the definition, once in the use.
;; Only the macro-introduced names are tinted: the cursor and x. p is a pattern
;; variable and m is a top-level binding, so neither was introduced by the macro.
(define tmpl-row1
  (list (ln (tint (code _cursor1234) INTRODUCED-COLOR))
        (t "nothing (unbound)")
        (code p)))
(define tmpl-row2
  (list (ln (tint (code _cursor1234) INTRODUCED-COLOR))
        (t "nothing (unbound)")
        (hbl-append (tint (code x) INTRODUCED-COLOR) (t ", ") (code m))))
(define (tmpl-table . rows)
  (table 3 (append tmpl-headers (apply append rows))
         lc-superimpose cc-superimpose 40 10))

;; tmpl-slide : caption program table -> slide, with a fixed layout so only
;; the boxes and the table's rows change between steps
(define (tmpl-slide caption program table)
  (slide #:title "Services in Templates" #:layout 'top
         caption
         program
         hygiene-legend
         (blank 20)
         table))

(tmpl-slide
 (cap "Insert a cursor" (code _cursor1234) "in the template body, then"
      "expand.")
 tmpl-prog-A
 (tmpl-table))

(tmpl-slide
 (cap "First we expand the definition, and reach the cursor sitting in the"
      "template.")
 (annotate-box tmpl-prog-A tmpl-a-cur)
 (tmpl-table))

(tmpl-slide
 (cap "Scanning a template, the only names we know are the pattern variables."
      "So all we learn here is that" (code p) "is in scope.")
 (annotate-box tmpl-prog-A tmpl-a-cur)
 (tmpl-table tmpl-row1))

(tmpl-slide
 (cap "Now we move on to the use.")
 (annotate-box tmpl-prog-A tmpl-call)
 (tmpl-table tmpl-row1))

(tmpl-slide
 (cap "Expanding it, we get another instance of the cursor identifier.")
 (annotate-box tmpl-prog-B tmpl-use-expansion)
 (tmpl-table tmpl-row1))

(tmpl-slide
 (cap "When resolving it again," (code x) "and" (code m) "are in scope, but" (code y) "is not:"
      "the cursor came from the macro, so hygiene keeps the use site's"
      "bindings away from it.")
 (annotate-box tmpl-prog-B tmpl-b-cur)
 (tmpl-table tmpl-row1))

(tmpl-slide
 (cap "One reference, two resolutions. Autocomplete takes the union of what"
      "was in scope for each:" (codep m ",") (codep p ",") (codep x "."))
 (annotate-box tmpl-prog-B tmpl-b-cur)
 (tmpl-table tmpl-row1 tmpl-row2))

;; ---------------------------------------------------------------------------
;; Limitations
;; ---------------------------------------------------------------------------

(slide
 #:title "Limitations: SSE in Incomplete Context"
 (para "SSE expands subexpressions in the context of the use")
 (item "No local bindings from the use")
 (item "No syntax parameters established by the macro")
 (item "No side effects from the macro"))

;; Two versions of my-cond with the same misuse — the first clause is missing
;; its body. Red marks the misuse. Each ~var annotation gets its own color,
;; shared with the fragment of the use SSE expands for it; an annotation with
;; no matching fragment (body) stays unpaired.
(define cond-bad-a-cond (code (~var condition expr)))
(define cond-bad-a-body (code (~var body expr)))
(define cond-bad-c1 (code (> x 0)))
(define cond-bad-clause1 (code [#,cond-bad-c1]))
(define cond-bad-prog
  (code
   (define-syntax my-cond
     (syntax-rules ()
       [(my-cond [#,cond-bad-a-cond
                  #,cond-bad-a-body]
                 clause ...)
        (if condition body (my-cond clause ...))]))
   code:blank
   (my-cond #,cond-bad-clause1
            [(< x 0) (- x)])))
(define cond-bad-scene
  (annotate-box (annotate-pairs
                 cond-bad-prog
                 (list (cons ANNOT-COLOR (list cond-bad-a-cond cond-bad-c1))
                       (cons ANNOT-COLOR-2 (list cond-bad-a-body))))
                cond-bad-clause1 #:color "red"))

(define cond-good-a-cond (code (~var condition expr)))
(define cond-good-a-body (code (~var body expr)))
(define cond-good-a-rcond (code (~var rest-conditions expr)))
(define cond-good-a-rbody (code (~var rest-bodies expr)))
(define cond-good-c1 (code (> x 0)))
(define cond-good-clause1 (code [#,cond-good-c1]))
(define cond-good-c2 (code (< x 0)))
(define cond-good-b2 (code (- x)))
(define cond-good-prog
  (code
   (define-syntax my-cond
     (syntax-rules ()
       [(my-cond [#,cond-good-a-cond
                  #,cond-good-a-body]
                 [#,cond-good-a-rcond
                  #,cond-good-a-rbody]
                 ...)
        (if condition body
            (my-cond
             [rest-conditions rest-bodies] ...))]))
   code:blank
   (my-cond #,cond-good-clause1
            [#,cond-good-c2 #,cond-good-b2])))
(define cond-good-scene
  (annotate-box (annotate-pairs
                 cond-good-prog
                 (list (cons ANNOT-COLOR (list cond-good-a-cond cond-good-c1))
                       (cons ANNOT-COLOR-2 (list cond-good-a-body))
                       (cons ANNOT-COLOR-3 (list cond-good-a-rcond cond-good-c2))
                       (cons ANNOT-COLOR-4 (list cond-good-a-rbody cond-good-b2))))
                cond-good-clause1 #:color "red"))

(slide
 #:title "Limitations: SSE with Recursive Macros"
 #:layout 'top
 cond-bad-scene

 (item "The first clause has no body, bad syntax.")
 (item "No SSE on the second clause because we never recursively call" (code my-cond)))

(slide
 #:title "Limitations: SSE with Recursive Macros"
 #:layout 'top
 cond-good-scene
 (t "Better annotations means Better SSE"))

(slide
 #:title "Limitations: SSE Can Misalign"
 (code
  (define-syntax m
    (syntax-rules ()
      [(m 1 2 (~var e expr)) 1]))
  (m 1 (let ([y 4]) y)))
 (blank 10)
 (item "No services on the" (code let) "because treason thinks it's supposed to be the" (code 2))
 (item "Treason doesn't distinguish between missing vs wrong vs extra"))

(slide
 #:title "Open Questions & Future Work"
 (item "Procedural macros")
 (subitem "Side effects")
 (subitem "Runtime support for fault-tolerance and SSE")
 (item "Fault-tolerant reading (missing parens are still fatal)")
 (item "Incremental re-expansion on edits")
 (item "Binding declaration like" (code syntax-spec)))

;; ---------------------------------------------------------------------------
;; syntax-spec vision
;; ---------------------------------------------------------------------------

(define match-use-code
  (code
   (define (sum nums)
     (my-match nums
       [(cons num nums)
        (+ num (sum nums))]
       [_ 0]))))

;; A trimmed version of the real match DSL's syntax-spec declaration
;; (syntax-spec/tests/dsls/match.rkt), cut down to the patterns this example
;; uses. A pattern exports the variables it binds; a clause imports them into
;; a scope around its body. The host interface for my-match is left out.
;; match-spec-code : Boolean -> pict
;; The declaration, with the #:binding clauses faded when dim? so the grammar
;; can be read on its own before the binding rules arrive.
(define (match-spec-code dim?)
  (define (b p) (if dim? (cellophane p 0.25) p))
  (define bind-var (b (code #:binding (export x))))
  (define bind-cons (b (code #:binding [(re-export p1) (re-export p2)])))
  (define bind-clause (b (code #:binding (scope (import p) body))))
  (code
   (syntax-spec
    (binding-class pat-var)

    (nonterminal/exporting pat
      x:pat-var
      #,bind-var
      _
      (cons p1:pat p2:pat)
      #,bind-cons)

    (nonterminal clause
      [p:pat body:racket-expr]
      #,bind-clause))))

;; The binder and the reference share the fill annotate-fill uses elsewhere for
;; a resolution, so the pair reads as "this num resolves to that num".
(define match-bad-binder (code num))
(define match-bad-ref (code num))
(define match-bad-code
  (annotate-fill
   (code
    (define (sum nums)
      (my-match nums
        [#,(framed-code (cons #,match-bad-binder))
         (+ #,match-bad-ref (sum nums))]
        [_ 0])))
   (list match-bad-binder match-bad-ref)))

(slide
 #:title "Binding Declarations"
 #:layout 'top
 (t "A pattern-matching DSL")
 match-use-code)

(slide
 #:title "Binding Declarations"
 #:layout 'top
 (t "The DSL author declares the grammar and the binding rules")
 (match-spec-code #t))

(slide
 #:title "Binding Declarations"
 #:layout 'top
 (t "The DSL author declares the grammar and the binding rules")
 (match-spec-code #f)
 (t "These binding rules can be used for SSE!")
 )

(slide
 #:title "Binding Declarations"
 #:layout 'top
 match-bad-code
 (para "With binding rules, SSE could know that the pattern variables are in scope"))

;; ---------------------------------------------------------------------------
;; End
;; ---------------------------------------------------------------------------

(slide
 #:title "Acknowledgements"
 (item "Michael Ballantyne")
 (item "The Racket community"))

;; regenerate the blog QR each build; simple-qr isn't a treason dependency,
;; so keep it out of info.rkt and assume it's installed locally
(define blog-qr-path (path->string (build-path here "blog-qr.png")))
(qr-write "https://www.youtube.com/@QuasarBrightYT" blog-qr-path)
(define blog-qr (scale (bitmap (read-bitmap blog-qr-path)) 0.5))

(slide
 #:title "Treason"
 (t "The language and language server are on GitHub")
 (hyperlinkize (t "https://github.com/quasarbright/treason"))
 (blank 20)
 (t "My Math and PL YouTube Channel")
 (hyperlinkize (t "https://www.youtube.com/@QuasarBrightYT"))
 blog-qr)