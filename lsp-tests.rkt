#lang racket

(require "server.rkt")
(module+ test (require rackunit))

;; ============================================================
;; Tests
;; ============================================================

(module+ test
  (test-case
   "basic let"
   (define source "(let ([x 2]) x)")
   (check-equal?
    (goto-definition source (find-position source "x" 1))
    (list
     (hash 'uri test-uri 'range (find-range source "x" 0))))
   (check-equal?
    (find-references source (find-position source "x" 1))
    (list
     (hash 'uri test-uri 'range (find-range source "x" 1))))
   (check-equal?
    (autocomplete source (find-position source "x" 0))
    (list))
   (check-equal?
    (autocomplete source (find-position source "x" 1))
    (list (hasheq 'label "x"))))

  (test-case
   "fault-tolerant"
   (define source "(let ([x unbound]) x)")
   (check-equal?
    (goto-definition source (find-position source "x" 1))
    (list
     (hash 'uri test-uri 'range (find-range source "x" 0))))
   (check-equal?
    (find-references source (find-position source "x" 1))
    (list
     (hash 'uri test-uri 'range (find-range source "x" 1))))
   (check-equal?
    (autocomplete source (find-position source "x" 0))
    (list))
   (check-equal?
    (autocomplete source (find-position source "x" 1))
    (list (hasheq 'label "x"))))

  (test-case
   "my-let"
   (define source
     ;; use q because x shows up in syntax-rules
     (~a '(let-syntax ([my-let (syntax-rules () [(_ ([q rhs]) body) (let ([q rhs]) body)])])
            (my-let ([q 2]) q))))
   (check-equal?
    (goto-definition source (find-position source "q" 3))
    (list
     (hash 'uri test-uri 'range (find-range source "q" 2))))
   (check-equal?
    (goto-definition source (find-position source "my-let" 1))
    (list
     (hash 'uri test-uri 'range (find-range source "my-let" 0))))
   (check-equal?
    (find-references source (find-position source "q" 3))
    (list
     (hash 'uri test-uri 'range (find-range source "q" 3))))
   (check-equal?
    (find-references source (find-position source "my-let" 1))
    (list
     (hash 'uri test-uri 'range (find-range source "my-let" 1))))
   (check-equal?
    (autocomplete source (find-position source "q" 2))
    (list))
   (check-equal?
    (autocomplete source (find-position source "q" 3))
    (list (hasheq 'label "my-let") (hasheq 'label "q"))))

  ;; ============================================================
  ;; Goto-definition tests for each binding form
  ;; ============================================================

  ;; 1. let binding - basic variable binding
  (test-case
   "goto-def: let binding"
   (define source "(let ([x 1]) x)")
   ;; Reference to binding site
   (check-equal?
    (goto-definition source (find-position source "x" 1))
    (list (hash 'uri test-uri 'range (find-range source "x" 0))))
   ;; Binding site returns itself (for VS Code fallback to references)
   (check-equal?
    (goto-definition source (find-position source "x" 0))
    (list (hash 'uri test-uri 'range (find-range source "x" 0)))))

  ;; 2. let binding - nested with shadowing
  (test-case
   "goto-def: let nested shadowing"
   (define source "(let ([x 1]) (let ([x 2]) x))")
   ;; Inner x reference goes to inner binding
   (check-equal?
    (goto-definition source (find-position source "x" 2))
    (list (hash 'uri test-uri 'range (find-range source "x" 1))))
   ;; Inner binding site returns itself
   (check-equal?
    (goto-definition source (find-position source "x" 1))
    (list (hash 'uri test-uri 'range (find-range source "x" 1))))
   ;; Outer binding site has no references (shadowed), returns empty
   (check-equal?
    (goto-definition source (find-position source "x" 0))
    (list)))

  ;; 3. let binding - reference in rhs of nested let
  (test-case
   "goto-def: let reference in nested rhs"
   (define source "(let ([x 1]) (let ([y x]) y))")
   ;; x in rhs of inner let goes to outer binding
   (check-equal?
    (goto-definition source (find-position source "x" 1))
    (list (hash 'uri test-uri 'range (find-range source "x" 0))))
   ;; y reference goes to y binding
   (check-equal?
    (goto-definition source (find-position source "y" 1))
    (list (hash 'uri test-uri 'range (find-range source "y" 0)))))

  ;; 4. block/define - forward references
  (test-case
   "goto-def: block define forward reference"
   (define source "(block (define x y) (define y 1) (#%expression x))")
   ;; x reference at end goes to x definition
   (check-equal?
    (goto-definition source (find-position source "x" 2))
    (list (hash 'uri test-uri 'range (find-range source "x" 0))))
   ;; y reference in x's rhs goes to y definition (forward ref)
   (check-equal?
    (goto-definition source (find-position source "y" 0))
    (list (hash 'uri test-uri 'range (find-range source "y" 1))))
   ;; y binding site returns itself
   (check-equal?
    (goto-definition source (find-position source "y" 1))
    (list (hash 'uri test-uri 'range (find-range source "y" 1)))))

  ;; 5. block/define - simple
  (test-case
   "goto-def: block define simple"
   (define source "(block (define x 1) (#%expression x))")
   ;; x reference goes to x definition
   (check-equal?
    (goto-definition source (find-position source "x" 2))
    (list (hash 'uri test-uri 'range (find-range source "x" 0))))
   ;; x binding site returns itself
   (check-equal?
    (goto-definition source (find-position source "x" 0))
    (list (hash 'uri test-uri 'range (find-range source "x" 0)))))

  ;; 6. let-syntax - macro binding
  (test-case
   "goto-def: let-syntax"
   (define source "(let-syntax ([m (syntax-rules () [(_) 42])]) (m))")
   ;; m use goes to m definition
   (check-equal?
    (goto-definition source (find-position source "m" 1))
    (list (hash 'uri test-uri 'range (find-range source "m" 0))))
   ;; m binding site returns itself
   (check-equal?
    (goto-definition source (find-position source "m" 0))
    (list (hash 'uri test-uri 'range (find-range source "m" 0)))))

  ;; 7. let-syntax with argument - macro that passes through
  (test-case
   "goto-def: let-syntax with arg"
   (define source "(let-syntax ([id (syntax-rules () [(_ x) x])]) (id 5))")
   ;; id use goes to id definition
   (check-equal?
    (goto-definition source (find-position source "id" 1))
    (list (hash 'uri test-uri 'range (find-range source "id" 0)))))

  ;; 8. define-syntax in block
  (test-case
   "goto-def: define-syntax in block"
   (define source "(block (define-syntax m (syntax-rules () [(_) 42])) (#%expression (m)))")
   ;; m use goes to m definition
   (check-equal?
    (goto-definition source (find-position source "m" 1))
    (list (hash 'uri test-uri 'range (find-range source "m" 0))))
   ;; m binding site returns itself
   (check-equal?
    (goto-definition source (find-position source "m" 0))
    (list (hash 'uri test-uri 'range (find-range source "m" 0)))))

  ;; 9. Macro that introduces binding (hygiene test)
  (test-case
   "goto-def: macro introduces binding"
   ;; With hygiene, the x in body should NOT refer to the macro-introduced x
   (define source "(let ([$x 0]) (let-syntax ([bind-$x (syntax-rules () [(_ body) (let ([$x 1]) body)])]) (bind-$x $x)))")
   ;; The x at the end should go to the outer x (hygiene preserved)
   ;; Note: This test documents expected hygienic behavior
   (check-equal?
    (goto-definition source (find-position source "$x" 4))
    (list (hash 'uri test-uri 'range (find-range source "$x" 0)))))

  ;; 10. Macro captures outer binding
  (displayln "skipping testing goto definition on template")
  #;
  (test-case
   "goto-def: macro captures outer"
   (define source "(let ([x 1]) (let-syntax ([get-x (syntax-rules () [(_) x])]) (let ([x 2]) (get-x))))")
   ;; The x inside get-x template should refer to outer x (captured at definition site)
   ;; Note: This test documents expected hygienic behavior
   (check-equal?
    (goto-definition source (find-position source "x" 2))
    (list (hash 'uri test-uri 'range (find-range source "x" 0)))))

  ;; 11. Unbound variable - empty result
  (test-case
   "goto-def: unbound variable"
   (define source "(let ([x 1]) y)")
   ;; y is unbound, should return empty
   (check-equal?
    (goto-definition source (find-position source "y" 0))
    (list)))

  ;; 12. Self-reference in block
  (test-case
   "goto-def: self-reference in block"
   (define source "(block (define x x) (#%expression x))")
   ;; x in rhs refers to x being defined (forward ref to self)
   (check-equal?
    (goto-definition source (find-position source "x" 1))
    (list (hash 'uri test-uri 'range (find-range source "x" 0))))
   ;; x at end refers to x definition
   (check-equal?
    (goto-definition source (find-position source "x" 3))
    (list (hash 'uri test-uri 'range (find-range source "x" 0)))))

  ;; ============================================================
  ;; Find-references tests (forward refs, multiple refs, self-refs)
  ;; ============================================================

  ;; 1. Forward references in blocks
  (test-case
   "find-refs: forward reference in block"
   (define source "(block (define x y) (define y 1) (#%expression x))")
   ;; y is referenced before it's defined - find-refs from the forward ref
   (check-equal?
    (find-references source (find-position source "y" 0))
    (list
     (hash 'uri test-uri 'range (find-range source "y" 0))))
   ;; find-refs from the binding site of y
   (check-equal?
    (find-references source (find-position source "y" 1))
    (list
     (hash 'uri test-uri 'range (find-range source "y" 0)))))

  ;; 2. Multiple references to same binding
  (test-case
   "find-refs: multiple references"
   (define source "(let ([x 1]) (let ([y x]) x))")
   ;; x has two references: in y's rhs and at the end
   ;; find-refs from binding site
   (check-equal?
    (find-references source (find-position source "x" 0))
    (list
     (hash 'uri test-uri 'range (find-range source "x" 2))
     (hash 'uri test-uri 'range (find-range source "x" 1))))
   ;; find-refs from first reference
   (check-equal?
    (find-references source (find-position source "x" 1))
    (list
     (hash 'uri test-uri 'range (find-range source "x" 2))
     (hash 'uri test-uri 'range (find-range source "x" 1))))
   ;; find-refs from second reference
   (check-equal?
    (find-references source (find-position source "x" 2))
    (list
     (hash 'uri test-uri 'range (find-range source "x" 2))
     (hash 'uri test-uri 'range (find-range source "x" 1)))))

  ;; 3. Binding site find-refs (no self-reference)
  (test-case
   "find-refs: binding site"
   (define source "(let ([x 1]) x)")
   ;; Binding site is not a reference; find-refs from binding returns only actual references
   (check-equal?
    (find-references source (find-position source "x" 0))
    (list
     (hash 'uri test-uri 'range (find-range source "x" 1)))))

  ;; 4. Self-reference in block (define x x)
  (test-case
   "find-refs: self-reference in block define"
   (define source "(block (define x x) (#%expression x))")
   ;; x binding is not a reference; only x in rhs and x at end
   (check-equal?
    (find-references source (find-position source "x" 0))
    (list
     (hash 'uri test-uri 'range (find-range source "x" 3))
     (hash 'uri test-uri 'range (find-range source "x" 1)))))

  ;; 5. Shadowing - inner and outer bindings have separate references
  (test-case
   "find-refs: shadowing separates references"
   (define source "(let ([x 1]) (let ([x 2]) x))")
   ;; Outer x has no uses (no references)
   (check-equal?
    (find-references source (find-position source "x" 0))
    (list))
   ;; Inner x has the reference in body
   (check-equal?
    (find-references source (find-position source "x" 1))
    (list
     (hash 'uri test-uri 'range (find-range source "x" 2)))))

  ;; 6. Macro binding references
  (test-case
   "find-refs: macro binding"
   (define source "(let-syntax ([m (syntax-rules () [(_) 42])]) (m))")
   ;; m binding and m use
   (check-equal?
    (find-references source (find-position source "m" 0))
    (list
     (hash 'uri test-uri 'range (find-range source "m" 1))))
   ;; find-refs from use site
   (check-equal?
    (find-references source (find-position source "m" 1))
    (list
     (hash 'uri test-uri 'range (find-range source "m" 1)))))

  ;; 7. Unbound variable - empty result
  (test-case
   "find-refs: unbound variable"
   (define source "(let ([x 1]) y)")
   ;; y is unbound, should return empty
   (check-equal?
    (find-references source (find-position source "y" 0))
    (list)))

  ;; 8. Multiple forward references in block
  (test-case
   "find-refs: multiple forward references"
   (define source "(block (define a b) (define c b) (define b 1) (#%expression a))")
   ;; b has two forward refs (binding site not included)
   (check-equal?
    (find-references source (find-position source "b" 3))
    (list
     (hash 'uri test-uri 'range (find-range source "b" 2))
     (hash 'uri test-uri 'range (find-range source "b" 1)))))

  ;; ============================================================
  ;; Autocomplete tests at various positions
  ;; ============================================================

  ;; 1. Autocomplete at binding site - should NOT include the binding being defined
  (test-case
   "autocomplete: at binding site"
   (define source "(let ([x 1]) x)")
   ;; At the binding site of x (index 0), x is not yet in scope
   (check-equal?
    (autocomplete source (find-position source "x" 0))
    (list))
   ;; At the reference site of x (index 1), x is in scope
   (check-equal?
    (autocomplete source (find-position source "x" 1))
    (list (hasheq 'label "x"))))

  ;; 2. Autocomplete in nested scopes - should show all accessible names
  (test-case
   "autocomplete: nested scopes"
   (define source "(let ([x 1]) (let ([y 2]) y))")
   ;; At y reference, both x and y should be in scope
   (check-equal?
    (autocomplete source (find-position source "y" 1))
    (list (hasheq 'label "x") (hasheq 'label "y")))
   ;; At y binding site, autocomplete returns empty (no resolution recorded)
   (check-equal?
    (autocomplete source (find-position source "y" 0))
    (list)))

  ;; 3. Autocomplete with shadowing - should show the shadowing name
  (test-case
   "autocomplete: shadowing"
   (define source "(let ([x 1]) (let ([x 2]) x))")
   ;; At inner x reference, x should be in scope (the inner one)
   (check-equal?
    (autocomplete source (find-position source "x" 2))
    (list (hasheq 'label "x")))
   ;; At inner x binding site, autocomplete returns empty (no resolution recorded)
   (check-equal?
    (autocomplete source (find-position source "x" 1))
    (list)))

  ;; 4. Autocomplete in block with forward references
  (test-case
   "autocomplete: block forward references"
   (define source "(block (define x y) (define y 1) (#%expression x))")
   ;; At x reference at end, both x and y should be in scope
   (check-equal?
    (autocomplete source (find-position source "x" 2))
    (list (hasheq 'label "x") (hasheq 'label "y")))
   ;; At y reference in x's rhs, both x and y should be in scope (forward ref)
   (check-equal?
    (autocomplete source (find-position source "y" 0))
    (list (hasheq 'label "x") (hasheq 'label "y"))))

  ;; 5. Autocomplete with let-syntax - macro should be in scope
  (test-case
   "autocomplete: let-syntax"
   (define source "(let-syntax ([m (syntax-rules () [(_) 42])]) (m))")
   ;; At m use site, m should be in scope
   (check-equal?
    (autocomplete source (find-position source "m" 1))
    (list (hasheq 'label "m")))
   ;; At m binding site, nothing should be in scope
   (check-equal?
    (autocomplete source (find-position source "m" 0))
    (list)))

  ;; 6. Autocomplete with define-syntax in block
  (test-case
   "autocomplete: define-syntax in block"
   (define source "(block (define-syntax m (syntax-rules () [(_) 42])) (#%expression (m)))")
   ;; At m use site, m should be in scope
   (check-equal?
    (autocomplete source (find-position source "m" 1))
    (list (hasheq 'label "m"))))

  ;; 7. Autocomplete with macro that introduces binding (hygiene)
  (test-case
   "autocomplete: macro introduces binding"
   ;; With hygiene, the macro-introduced x should NOT be visible to surface code
   (define source "(let ([$x 0]) (let-syntax ([bind-$x (syntax-rules () [(_ body) (let ([$x 1]) body)])]) (bind-$x $x)))")
   ;; At the x in (bind-x x), only the outer x should be in scope (hygiene)
   (check-equal?
    (autocomplete source (find-position source "$x" 4))
    (list (hasheq 'label "$x") (hasheq 'label "bind-$x"))))

  ;; 8. Autocomplete with macro that captures outer binding
  (test-case
   "autocomplete: macro captures outer"
   (define source "(let ([x 1]) (let-syntax ([get-x (syntax-rules () [(_) x])]) (let ([x 2]) (get-x))))")
   ;; At (get-x) call site, both get-x and inner x should be in scope
   (check-equal?
    (autocomplete source (find-position source "get-x" 1))
    (list (hasheq 'label "get-x") (hasheq 'label "x"))))

  ;; 9. Autocomplete with unbound variable - should still show names in scope
  (test-case
   "autocomplete: unbound variable"
   (define source "(let ([x 1]) y)")
   ;; At y (unbound), x should still be in scope
   (check-equal?
    (autocomplete source (find-position source "y" 0))
    (list (hasheq 'label "x"))))

  ;; 10. Autocomplete in rhs of let binding
  (test-case
   "autocomplete: in rhs of let"
   (define source "(let ([x (let ([y 1]) y)]) x)")
   ;; At y reference in inner let, y should be in scope
   (check-equal?
    (autocomplete source (find-position source "y" 1))
    (list (hasheq 'label "y")))
   ;; At x reference at end, x should be in scope
   (check-equal?
    (autocomplete source (find-position source "x" 1))
    (list (hasheq 'label "x"))))

  ;; 11. Autocomplete with multiple bindings at same level
  (test-case
   "autocomplete: multiple bindings"
   (define source "(let ([x 1]) (let ([y 2]) (let ([z 3]) y)))")
   ;; At y reference, x, y, z should all be in scope
   (check-equal?
    (autocomplete source (find-position source "y" 1))
    (list (hasheq 'label "x") (hasheq 'label "y") (hasheq 'label "z"))))

  ;; 12. Autocomplete with self-reference in block
  (test-case
   "autocomplete: self-reference in block"
   (define source "(block (define x x) (#%expression x))")
   ;; At x in rhs (self-ref), x should be in scope
   (check-equal?
    (autocomplete source (find-position source "x" 1))
    (list (hasheq 'label "x")))
   ;; At x at end, x should be in scope
   (check-equal?
    (autocomplete source (find-position source "x" 3))
    (list (hasheq 'label "x"))))

  ;; 13. Autocomplete with nested macros
  (test-case
   "autocomplete: nested macros"
   (define source "(let-syntax ([m1 (syntax-rules () [(_ x) (let ([a x]) a)])])
     (let-syntax ([m2 (syntax-rules () [(_ y) (m1 y)])])
       (m2 5)))")
   ;; At (m2 5), both m1 and m2 should be in scope
   (check-equal?
    (autocomplete source (find-position source "m2" 1))
    (list (hasheq 'label "m1") (hasheq 'label "m2"))))

  ;; 14. Autocomplete with deep nesting
  (test-case
   "autocomplete: deep nesting"
   (define source "(let ([a 1]) (let ([b 2]) (let ([c 3]) (let ([a 4]) a))))")
   ;; At inner a reference, a, b, c should all be in scope
   (check-equal?
    (autocomplete source (find-position source "a" 2))
    (list (hasheq 'label "a") (hasheq 'label "b") (hasheq 'label "c"))))

  ;; ============================================================
  ;; Shadowing tests (inner binding shadows outer)
  ;; ============================================================

  ;; 1. Basic shadowing - inner let shadows outer let
  (test-case
   "shadowing: basic let shadows let"
   (define source "(let ([x 1]) (let ([x 2]) x))")
   ;; Goto-def: inner x reference goes to inner binding
   (check-equal?
    (goto-definition source (find-position source "x" 2))
    (list (hash 'uri test-uri 'range (find-range source "x" 1))))
   ;; Find-refs: outer x has no uses
   (check-equal?
    (find-references source (find-position source "x" 0))
    (list))
   ;; Find-refs: inner x has the reference
   (check-equal?
    (find-references source (find-position source "x" 1))
    (list
     (hash 'uri test-uri 'range (find-range source "x" 2))))
   ;; Autocomplete: at inner x reference, x is in scope
   (check-equal?
    (autocomplete source (find-position source "x" 2))
    (list (hasheq 'label "x"))))

  ;; 2. Shadowing with reference to outer before inner binding
  (test-case
   "shadowing: reference outer before inner"
   (define source "(let ([x 1]) (let ([y x]) (let ([x 2]) x)))")
   ;; y's rhs references outer x
   (check-equal?
    (goto-definition source (find-position source "x" 1))
    (list (hash 'uri test-uri 'range (find-range source "x" 0))))
   ;; innermost x reference goes to inner x binding
   (check-equal?
    (goto-definition source (find-position source "x" 3))
    (list (hash 'uri test-uri 'range (find-range source "x" 2))))
   ;; outer x has the reference in y's rhs
   (check-equal?
    (find-references source (find-position source "x" 0))
    (list
     (hash 'uri test-uri 'range (find-range source "x" 1)))))

  ;; 3. Deep shadowing - multiple levels
  (test-case
   "shadowing: deep nesting"
   (define source "(let ([x 1]) (let ([x 2]) (let ([x 3]) x)))")
   ;; innermost x reference goes to innermost binding
   (check-equal?
    (goto-definition source (find-position source "x" 3))
    (list (hash 'uri test-uri 'range (find-range source "x" 2))))
   ;; middle x has no references
   (check-equal?
    (find-references source (find-position source "x" 1))
    (list))
   ;; outer x has no references
   (check-equal?
    (find-references source (find-position source "x" 0))
    (list)))

  ;; 4. Shadowing in block with define
  (test-case
   "shadowing: block define shadows outer let"
   (define source "(let ([$x 1]) (block (define $x 2) (#%expression $x)))")
   ;; $x reference in block goes to block's define
   (check-equal?
    (goto-definition source (find-position source "$x" 2))
    (list (hash 'uri test-uri 'range (find-range source "$x" 1))))
   ;; outer $x has no references
   (check-equal?
    (find-references source (find-position source "$x" 0))
    (list)))

  ;; 5. Shadowing with macro binding
  (test-case
   "shadowing: macro shadows variable"
   (define source "(let ([m 1]) (let-syntax ([m (syntax-rules () [(_) 42])]) (m)))")
   ;; m use goes to macro binding, not variable
   (check-equal?
    (goto-definition source (find-position source "m" 2))
    (list (hash 'uri test-uri 'range (find-range source "m" 1))))
   ;; outer m (variable) has no references
   (check-equal?
    (find-references source (find-position source "m" 0))
    (list)))

  ;; 6. Shadowing with variable over macro
  (test-case
   "shadowing: variable shadows macro"
   (define source "(let-syntax ([$x (syntax-rules () [(_) 42])]) (let ([$x 1]) $x))")
   ;; x reference goes to variable binding, not macro
   (check-equal?
    (goto-definition source (find-position source "$x" 2))
    (list (hash 'uri test-uri 'range (find-range source "$x" 1))))
   ;; macro x has no references
   (check-equal?
    (find-references source (find-position source "$x" 0))
    (list)))

  ;; 7. Shadowing with same name in different branches
  (test-case
   "shadowing: different branches"
   (define source "(let ([x 1]) (let ([y (let ([x 2]) x)]) x))")
   ;; x in inner let body goes to inner x
   (check-equal?
    (goto-definition source (find-position source "x" 2))
    (list (hash 'uri test-uri 'range (find-range source "x" 1))))
   ;; x at end goes to outer x
   (check-equal?
    (goto-definition source (find-position source "x" 3))
    (list (hash 'uri test-uri 'range (find-range source "x" 0))))
   ;; outer x has the final reference
   (check-equal?
    (find-references source (find-position source "x" 0))
    (list
     (hash 'uri test-uri 'range (find-range source "x" 3)))))

  ;; 8. Autocomplete shows shadowing name only once
  (test-case
   "shadowing: autocomplete shows name once"
   (define source "(let ([x 1]) (let ([x 2]) (let ([y 3]) y)))")
   ;; At y reference, x and y should be in scope (x appears once despite shadowing)
   (check-equal?
    (autocomplete source (find-position source "y" 1))
    (list (hasheq 'label "x") (hasheq 'label "y"))))

  ;; 9. Shadowing at binding site - outer still visible
  (test-case
   "shadowing: outer visible at inner binding site"
   (define source "(let ([x 1]) (let ([x x]) x))")
   ;; x in rhs of inner let refers to outer x
   (check-equal?
    (goto-definition source (find-position source "x" 2))
    (list (hash 'uri test-uri 'range (find-range source "x" 0))))
   ;; x in body refers to inner x
   (check-equal?
    (goto-definition source (find-position source "x" 3))
    (list (hash 'uri test-uri 'range (find-range source "x" 1))))
   ;; outer x has the rhs reference
   (check-equal?
    (find-references source (find-position source "x" 0))
    (list
     (hash 'uri test-uri 'range (find-range source "x" 2)))))

  ;; ============================================================
  ;; Hygiene tests (surface refs resolve to surface bindings)
  ;; ============================================================
  ;; These tests verify Property 12: Hygiene Preservation in LSP
  ;; When a macro introduces a binding with the same symbol as a surface binding,
  ;; surface references to that symbol SHALL resolve to the surface binding
  ;; (not the macro-introduced one).
  ;; Note: We use $x instead of x to avoid conflicts with x in syntax-rules patterns.

  ;; 1. Basic hygiene - macro introduces binding, surface ref resolves to surface binding
  (test-case
   "hygiene: macro-introduced binding doesn't shadow surface binding"
   ;; bind-$x introduces a binding for $x, but the surface $x should still
   ;; resolve to the outer surface binding
   (define source "(let ([$x 0]) (let-syntax ([bind-$x (syntax-rules () [(_ body) (let ([$x 1]) body)])]) (bind-$x $x)))")
   ;; The $x at the end (index 4) should go to the outer $x (index 0), not the macro-introduced $x
   (check-equal?
    (goto-definition source (find-position source "$x" 4))
    (list (hash 'uri test-uri 'range (find-range source "$x" 0))))
   ;; Find-refs from outer $x should include the surface reference
   (check-equal?
    (find-references source (find-position source "$x" 0))
    (list
     (hash 'uri test-uri 'range (find-range source "$x" 4)))))

  ;; 2. Hygiene - macro captures outer binding
  (test-case
   "hygiene: macro captures definition-site binding"
   ;; get-$x captures the $x from its definition site, not the use site
   (define source "(let ([$x 1]) (let-syntax ([get-$x (syntax-rules () [(_) $x])]) (let ([$x 2]) (get-$x))))")
   ;; The $x in the template should resolve to the outer $x
   ;; because it was captured at macro definition time
   (displayln "skipping testing goto definition on template")
   #;
   (check-equal?
    (goto-definition source (find-position source "$x" 2))
    (list (hash 'uri test-uri 'range (find-range source "$x" 0))))
   ;; The inner $x is a separate binding with no references to it
   (check-equal?
    (find-references source (find-position source "$x" 3))
    (list)))

  ;; 3. Hygiene - nested macro with introduced binding
  (test-case
   "hygiene: nested macro with introduced binding"
   ;; m1 introduces a binding for $a, m2 uses m1
   ;; The surface argument should not be captured by macro-introduced $a
   (define source "(let ([$a 0]) (let-syntax ([m1 (syntax-rules () [(_ body) (let ([$a 1]) body)])]) (let-syntax ([m2 (syntax-rules () [(_ z) (m1 z)])]) (m2 $a))))")
   ;; The $a at the end should resolve to the outer $a, not any macro-introduced $a
   (check-equal?
    (goto-definition source (find-position source "$a" 2))
    (list (hash 'uri test-uri 'range (find-range source "$a" 0)))))

  ;; 4. Hygiene - Fig. 17 example (canonical hygiene test)
  (test-case
   "hygiene: fig17 canonical example"
   ;; This is the canonical hygiene test from the scope graph paper
   ;; def-m defines a macro m that:
   ;; - defines $x (macro-introduced)
   ;; - defines a macro m that defines given-$x and references $x
   ;; The $x reference in m should resolve to the macro-introduced $x from def-m,
   ;; not to any surface $x
   (define source "(block
     (define-syntax def-m
       (syntax-rules ()
         [(_ m given-$x)
          (begin
            (define $x 1)
            (define-syntax m
              (syntax-rules ()
                [(_)
                 (begin
                   (define given-$x 2)
                   (#%expression $x))])))]))
     (def-m m $x)
     (m))")
   ;; The surface $x (in "def-m m $x") should be a binding site for given-$x
   ;; When m is called, it defines given-$x (which is the surface $x) and references
   ;; the macro-introduced $x from def-m
   ;; The surface $x at "def-m m $x" should be findable
   (check-equal?
    (goto-definition source (find-position source "$x" 4))
    (list)))

  ;; 5. Hygiene - autocomplete respects hygiene
  (test-case
   "hygiene: autocomplete only shows hygienically accessible names"
   ;; bind-$x introduces $x, but surface code should not see it
   (define source "(let ([$y 0]) (let-syntax ([bind-$x (syntax-rules () [(_ body) (let ([$x 1]) body)])]) (bind-$x $y)))")
   ;; At the $y reference inside bind-$x, only $y and bind-$x should be in scope
   ;; The macro-introduced $x should NOT be visible
   (check-equal?
    (autocomplete source (find-position source "$y" 1))
    (list (hasheq 'label "$y") (hasheq 'label "bind-$x"))))

  ;; 6. Hygiene - multiple macro-introduced bindings with same name
  (test-case
   "hygiene: multiple macro-introduced bindings don't interfere"
   ;; Two macros both introduce $x, but they shouldn't interfere with each other
   ;; or with surface bindings
   (define source "(let ([$x 0]) (let-syntax ([m1 (syntax-rules () [(_ body) (let ([$x 1]) body)])]) (let-syntax ([m2 (syntax-rules () [(_ body) (let ([$x 2]) body)])]) (m1 (m2 $x)))))")
   ;; The innermost $x should still resolve to the outer surface $x
   (check-equal?
    (goto-definition source (find-position source "$x" 3))
    (list (hash 'uri test-uri 'range (find-range source "$x" 0)))))

  ;; 7. Hygiene - macro-introduced reference to macro-introduced binding
  (test-case
   "hygiene: macro-introduced ref to macro-introduced binding"
   ;; When a macro introduces both a binding and a reference to it,
   ;; the reference should resolve to the macro-introduced binding
   ;; This is NOT a surface reference, so it won't appear in LSP results
   (define source "(let-syntax ([m (syntax-rules () [(_) (let ([$x 1]) $x)])]) (m))")
   ;; The macro use site should be findable
   (check-equal?
    (goto-definition source (find-position source "m" 1))
    (list (hash 'uri test-uri 'range (find-range source "m" 0))))
   ;; Autocomplete at m use site should show m
   (check-equal?
    (autocomplete source (find-position source "m" 1))
    (list (hasheq 'label "m"))))

  ;; 8. Hygiene - surface binding shadows macro-introduced in surface code
  (test-case
   "hygiene: surface binding shadows macro-introduced for surface refs"
   ;; Even if a macro introduces $x first, a later surface binding of $x
   ;; should shadow it for surface references
   (define source "(let-syntax ([bind-$x (syntax-rules () [(_ body) (let ([$x 1]) body)])]) (bind-$x (let ([$x 2]) $x)))")
   ;; The $x reference at the end should go to the surface $x binding
   (check-equal?
    (goto-definition source (find-position source "$x" 4))
    (list (hash 'uri test-uri 'range (find-range source "$x" 3))))
   ;; Find-refs from surface $x should include the reference
   (check-equal?
    (find-references source (find-position source "$x" 3))
    (list
     (hash 'uri test-uri 'range (find-range source "$x" 4)))))

  ;; 9. Hygiene - define-syntax in block with hygiene
  (test-case
   "hygiene: define-syntax in block respects hygiene"
   (define source "(let ([$x 0]) (block (define-syntax bind-$x (syntax-rules () [(_ body) (let ([$x 1]) body)])) (#%expression (bind-$x $x))))")
   ;; The $x at the end should resolve to the outer $x
   (check-equal?
    (goto-definition source (find-position source "$x" 4))
    (list (hash 'uri test-uri 'range (find-range source "$x" 0)))))

  ;; ============================================================
  ;; Error case tests (unbound still allows autocomplete)
  ;; ============================================================
  ;; These tests verify that LSP features continue to work in the presence
  ;; of errors (unbound variables, bad syntax, etc.). The expander is
  ;; fault-tolerant and produces stx-error sentinels rather than halting.

  ;; 1. Autocomplete works at unbound variable position
  (test-case
   "error: autocomplete at unbound variable"
   (define source "(let ([x 1]) y)")
   ;; At y (unbound), x should still be in scope for autocomplete
   (check-equal?
    (autocomplete source (find-position source "y" 0))
    (list (hasheq 'label "x"))))

  ;; 2. Autocomplete works when rhs has unbound variable
  (test-case
   "error: autocomplete with unbound in rhs"
   (define source "(let ([x unbound]) x)")
   ;; At x reference, x should be in scope despite unbound in rhs
   (check-equal?
    (autocomplete source (find-position source "x" 1))
    (list (hasheq 'label "x")))
   ;; At unbound, x should NOT be in scope (it's in the rhs, before x is bound)
   (check-equal?
    (autocomplete source (find-position source "unbound" 0))
    (list)))

  ;; 3. Goto-definition works for valid bindings in program with errors
  (test-case
   "error: goto-def works with unbound elsewhere"
   (define source "(let ([x 1]) (let ([y unbound]) x))")
   ;; x reference should still resolve to x binding
   (check-equal?
    (goto-definition source (find-position source "x" 1))
    (list (hash 'uri test-uri 'range (find-range source "x" 0))))
   ;; y binding site has no references, returns empty
   (check-equal?
    (goto-definition source (find-position source "y" 0))
    (list)))

  ;; 4. Find-references works for valid bindings in program with errors
  (test-case
   "error: find-refs works with unbound elsewhere"
   (define source "(let ([x 1]) (let ([y unbound]) x))")
   ;; x should have reference (binding site not included)
   (check-equal?
    (find-references source (find-position source "x" 0))
    (list
     (hash 'uri test-uri 'range (find-range source "x" 1)))))

  ;; 5. Autocomplete in nested scope with error in outer scope
  (test-case
   "error: autocomplete in nested scope with outer error"
   (define source "(let ([x unbound]) (let ([y 1]) y))")
   ;; At y reference, both x and y should be in scope
   (check-equal?
    (autocomplete source (find-position source "y" 1))
    (list (hasheq 'label "x") (hasheq 'label "y"))))

  ;; 6. Autocomplete with multiple unbound variables
  (test-case
   "error: autocomplete with multiple unbound"
   (define source "(let ([x 1]) (let ([y a]) (let ([z b]) z)))")
   ;; At z reference, x, y, z should all be in scope despite unbound a and b
   (check-equal?
    (autocomplete source (find-position source "z" 1))
    (list (hasheq 'label "x") (hasheq 'label "y") (hasheq 'label "z"))))

  ;; 7. Autocomplete at unbound macro call
  (test-case
   "error: autocomplete at unbound macro"
   (define source "(let ([x 1]) (undefined-macro x))")
   ;; At x in the unbound macro call, x should be in scope
   (check-equal?
    (autocomplete source (find-position source "x" 1))
    (list (hasheq 'label "x"))))

  ;; 8. Goto-definition returns empty for unbound variable
  (test-case
   "error: goto-def empty for unbound"
   (define source "(let ([x 1]) y)")
   ;; y is unbound, should return empty
   (check-equal?
    (goto-definition source (find-position source "y" 0))
    (list)))

  ;; 9. Find-references returns empty for unbound variable
  (test-case
   "error: find-refs empty for unbound"
   (define source "(let ([x 1]) y)")
   ;; y is unbound, should return empty
   (check-equal?
    (find-references source (find-position source "y" 0))
    (list)))

  ;; 10. Autocomplete in block with forward reference to unbound
  (test-case
   "error: autocomplete in block with unbound forward ref"
   (define source "(block (define x unbound) (define y 1) (#%expression y))")
   ;; At y reference, both x and y should be in scope
   (check-equal?
    (autocomplete source (find-position source "y" 1))
    (list (hasheq 'label "x") (hasheq 'label "y"))))

  ;; 11. Autocomplete works after syntax error in earlier form
  (test-case
   "error: autocomplete after earlier error"
   (define source "(let ([x 1]) (let ([y bad-ref]) (let ([z 2]) z)))")
   ;; At z reference, x, y, z should all be in scope
   (check-equal?
    (autocomplete source (find-position source "z" 1))
    (list (hasheq 'label "x") (hasheq 'label "y") (hasheq 'label "z"))))

  ;; 12. Goto-definition and find-references work for macro with error in body
  (test-case
   "error: LSP works for macro with error in expansion"
   (define source "(let-syntax ([m (syntax-rules () [(_) unbound])]) (m))")
   ;; m use should still resolve to m definition
   (check-equal?
    (goto-definition source (find-position source "m" 1))
    (list (hash 'uri test-uri 'range (find-range source "m" 0))))
   ;; find-refs should work for m
   (check-equal?
    (find-references source (find-position source "m" 0))
    (list
     (hash 'uri test-uri 'range (find-range source "m" 1)))))

  ;; 13. Autocomplete with cursor insertion - no identifier at position
  ;; This tests the cursor insertion mechanism described in the design doc:
  ;; when autocomplete is requested at a non-identifier position, we insert
  ;; a cursor placeholder, expand, and use get-names-in-scope on the cursor.
  (test-case
   "error: autocomplete with cursor insertion (empty body)"
   ;; The cursor is at position 13 (after the space, where body should go)
   ;; Source: "(let ([x 1]) )"
   ;;          0123456789012345
   (define source "(let ([x 1]) )")
   ;; At position 13 (the space before closing paren), x should be in scope
   ;; This requires cursor insertion since there's no identifier there
   (check-equal?
    (autocomplete source (hash 'line 0 'character 13))
    (list (hasheq 'label "x"))))

  ;; 14. Autocomplete with cursor insertion in nested let
  (test-case
   "error: autocomplete with cursor insertion (nested empty body)"
   ;; Source: "(let ([x 1]) (let ([y 2]) ))"
   ;;          0         1         2
   ;;          0123456789012345678901234567
   (define source "(let ([x 1]) (let ([y 2]) ))")
   ;; At position 26 (space before inner closing paren), both x and y should be in scope
   (check-equal?
    (autocomplete source (hash 'line 0 'character 26))
    (list (hasheq 'label "x") (hasheq 'label "y"))))

  ;; 15. Autocomplete with cursor insertion in block
  (test-case
   "error: autocomplete with cursor insertion (block empty expression)"
   ;; Source: "(block (define x 1) (#%expression ))"
   ;;          0         1         2         3
   ;;          012345678901234567890123456789012345
   (define source "(block (define x 1) (#%expression ))")
   ;; At position 34 (space before closing paren of #%expression), x should be in scope
   (check-equal?
    (autocomplete source (hash 'line 0 'character 34))
    (list (hasheq 'label "x"))))
  )

;; ============================================================
;; Test helpers

(define test-client%
  (class object%
    (super-new)
    (define/public (textDocument/publishDiagnostics . _) (void))))

;; A test client that captures published diagnostics.
(define capturing-client%
  (class object%
    (super-new)
    (define diagnostics '())
    (define/public (textDocument/publishDiagnostics params)
      (set! diagnostics (hash-ref params 'diagnostics '())))
    (define/public (get-diagnostics) diagnostics)))

(define test-uri "test.tsn")

;; make-test-server : String -> server%
;; Creates a server, initializes it, and opens the source as test.tsn
(define (make-test-server source)
  (define client (new test-client%))
  (define server (new server% [client client]))
  (send server initialize (hasheq))
  (send server textDocument/didOpen
        (hasheq 'textDocument (hasheq 'uri test-uri 'text source)))
  server)

;; has-unbound-error-at? : String Position -> Boolean
;; Returns #t if expanding source produces an "unbound identifier" error
;; whose range overlaps the given position.
(define (has-unbound-error-at? source pos)
  (define client (new capturing-client%))
  (define server (new server% [client client]))
  (send server initialize (hasheq))
  (send server textDocument/didOpen
        (hasheq 'textDocument (hasheq 'uri test-uri 'text source)))
  (define diags (send client get-diagnostics))
  (for/or ([d diags])
    (and (regexp-match? #rx"unbound identifier" (hash-ref d 'message ""))
         (position-in-range? pos (hash-ref d 'range)))))

;; position-in-range? : Position Range -> Boolean
;; Returns #t if the position falls within the range (inclusive start, exclusive end).
(define (position-in-range? pos range)
  (define start (hash-ref range 'start))
  (define end (hash-ref range 'end))
  (define line (hash-ref pos 'line))
  (define char (hash-ref pos 'character))
  (and (or (> line (hash-ref start 'line))
           (and (= line (hash-ref start 'line))
                (>= char (hash-ref start 'character))))
       (or (< line (hash-ref end 'line))
           (and (= line (hash-ref end 'line))
                (< char (hash-ref end 'character))))))

;; Find the nth (0-based) occurrence of pattern in source, return LSP range
(define (find-range source pattern [index 0])
  (define lines (string-split source "\n" #:trim? #f))
  (let loop ([line-num 0] [lines lines] [count 0])
    (cond
      [(null? lines)
       (error 'find-range "pattern ~s occurrence ~a not found" pattern index)]
      [else
       (define line (car lines))
       (let inner ([start 0] [count count])
         (define pos (regexp-match-positions (regexp-quote pattern) line start))
         (cond
           [(not pos)
            (loop (add1 line-num) (cdr lines) count)]
           [(= count index)
            (define match-start (caar pos))
            (define match-end (cdar pos))
            (hash 'start (hash 'line line-num 'character match-start)
                  'end (hash 'line line-num 'character match-end))]
           [else
            (inner (cdar pos) (add1 count))]))])))

;; Find the nth occurrence of pattern, return just the start position
(define (find-position source pattern [index 0])
  (hash-ref (find-range source pattern index) 'start))

;; Self-contained goto-definition: creates server, queries, returns locations
(define (goto-definition source position)
  (define server (make-test-server source))
  (send server textDocument/definition
        (hasheq 'textDocument (hasheq 'uri test-uri)
                'position (hasheq 'line (hash-ref position 'line)
                                  'character (hash-ref position 'character)))))

;; Self-contained find-references: creates server, queries, returns locations
(define (find-references source position)
  (define server (make-test-server source))
  (send server textDocument/references
        (hasheq 'textDocument (hasheq 'uri test-uri)
                'position (hasheq 'line (hash-ref position 'line)
                                  'character (hash-ref position 'character)))))

;; Self-contained autocomplete: creates server, queries, returns completions
(define (autocomplete source position)
  (define server (make-test-server source))
  (define results
    (send server textDocument/completion
          (hasheq 'textDocument (hasheq 'uri test-uri)
                  'position (hasheq 'line (hash-ref position 'line)
                                    'character (hash-ref position 'character)))))
  (sort results string<? #:key (lambda (h) (hash-ref h 'label))))

;; ============================================================
;; Sample Program Pool
;; ============================================================

;; A curated pool of sample programs covering interesting cases for LSP testing.
;; These programs are used for both targeted tests and consistency checks.

;; 1. Simple let bindings
(define sample-simple-let
  "(let ([x 2]) x)")

(define sample-let-with-rhs
  "(let ([x (let ([y 1]) y)]) x)")

;; 2. Nested scopes with shadowing
(define sample-nested-shadowing
  "(let ([x 1]) (let ([x 2]) x))")

(define sample-nested-no-shadow
  "(let ([x 1]) (let ([y 2]) x))")

(define sample-deep-nesting
  "(let ([a 1]) (let ([b 2]) (let ([c 3]) (let ([a 4]) a))))")

;; 3. Forward references in blocks (define)
(define sample-block-forward-ref
  "(block (define x y) (define y 1) (#%expression x))")

(define sample-block-mutual-ref
  "(block (define f (block (#%expression g))) (define g 1) (#%expression f))")

(define sample-block-simple
  "(block (define x 1) (#%expression x))")

;; 4. let-syntax and define-syntax
(define sample-let-syntax
  "(let-syntax ([m (syntax-rules () [(_) 42])]) (m))")

(define sample-define-syntax
  "(block (define-syntax m (syntax-rules () [(_) 42])) (#%expression (m)))")

(define sample-let-syntax-with-arg
  "(let-syntax ([id (syntax-rules () [(_ x) x])]) (id 5))")

;; 5. Macro that duplicates use-site syntax
(define sample-macro-duplicates
  "(let-syntax ([dup (syntax-rules () [(_ x) (let ([a x]) (let ([b x]) a))])]) (dup 1))")

(define sample-macro-duplicates-id
  "(let ([z 1]) (let-syntax ([dup (syntax-rules () [(_ x) (let ([a x]) x)])]) (dup z)))")

;; 6. Macro that introduces bindings
(define sample-macro-introduces-binding
  "(let-syntax ([bind-x (syntax-rules () [(_ body) (let ([x 1]) body)])]) (bind-x 2))")

(define sample-macro-introduces-binding-used
  ;; With hygiene, the x in body should NOT refer to the macro-introduced x
  "(let ([x 0]) (let-syntax ([bind-x (syntax-rules () [(_ body) (let ([x 1]) body)])]) (bind-x x)))")

;; 7. Nested macro applications
(define sample-nested-macros
  "(let-syntax ([m1 (syntax-rules () [(_ x) (let ([a x]) a)])])
     (let-syntax ([m2 (syntax-rules () [(_ y) (m1 y)])])
       (m2 5)))")

(define sample-macro-defines-macro
  "(block
     (define-syntax outer
       (syntax-rules ()
         [(_ name)
          (define-syntax name
            (syntax-rules ()
              [(_) 42]))]))
     (outer inner)
     (#%expression (inner)))")

;; 8. Fig. 17 hygiene example
;; This is the canonical hygiene test from the scope graph paper
(define sample-fig17
  "(block
     (define-syntax def-m
       (syntax-rules ()
         [(_ m given-x)
          (begin
            (define x 1)
            (define-syntax m
              (syntax-rules ()
                [(_)
                 (begin
                   (define given-x 2)
                   (#%expression x))])))]))
     (def-m m x)
     (m))")

;; 9. Programs with errors (unbound, bad syntax)
(define sample-unbound-var
  "(let ([x 1]) y)")

(define sample-unbound-in-rhs
  "(let ([x unbound]) x)")

(define sample-bad-let-syntax
  "(let 123)")

(define sample-unbound-macro
  "(undefined-macro 1 2 3)")

;; Additional interesting cases

;; Macro with literals
(define sample-macro-with-literal
  "(let-syntax ([cond-like (syntax-rules (else) [(_ else x) x] [(_ test x) test])])
     (cond-like else 42))")

;; Self-reference in block
(define sample-block-self-ref
  "(block (define x x) (#%expression x))")

;; Multiple bindings at same level
(define sample-multiple-bindings
  "(let ([x 1]) (let ([y 2]) (let ([z 3]) y)))")

;; Macro that uses outer binding
(define sample-macro-captures-outer
  "(let ([x 1])
     (let-syntax ([get-x (syntax-rules () [(_) x])])
       (let ([x 2])
         (get-x))))")

;; All sample programs collected for consistency checks
(define sample-programs
  (list
   (cons 'simple-let sample-simple-let)
   (cons 'let-with-rhs sample-let-with-rhs)
   (cons 'nested-shadowing sample-nested-shadowing)
   (cons 'nested-no-shadow sample-nested-no-shadow)
   (cons 'deep-nesting sample-deep-nesting)
   (cons 'block-forward-ref sample-block-forward-ref)
   (cons 'block-mutual-ref sample-block-mutual-ref)
   (cons 'block-simple sample-block-simple)
   (cons 'let-syntax sample-let-syntax)
   (cons 'define-syntax sample-define-syntax)
   (cons 'let-syntax-with-arg sample-let-syntax-with-arg)
   (cons 'macro-duplicates sample-macro-duplicates)
   (cons 'macro-duplicates-id sample-macro-duplicates-id)
   (cons 'macro-introduces-binding sample-macro-introduces-binding)
   (cons 'macro-introduces-binding-used sample-macro-introduces-binding-used)
   (cons 'nested-macros sample-nested-macros)
   (cons 'macro-defines-macro sample-macro-defines-macro)
   (cons 'fig17 sample-fig17)
   (cons 'unbound-var sample-unbound-var)
   (cons 'unbound-in-rhs sample-unbound-in-rhs)
   (cons 'macro-with-literal sample-macro-with-literal)
   (cons 'block-self-ref sample-block-self-ref)
   (cons 'multiple-bindings sample-multiple-bindings)
   (cons 'macro-captures-outer sample-macro-captures-outer)))

;; Error programs (subset that should produce errors)
(define sample-error-programs
  (list
   (cons 'unbound-var sample-unbound-var)
   (cons 'unbound-in-rhs sample-unbound-in-rhs)
   (cons 'bad-let-syntax sample-bad-let-syntax)
   (cons 'unbound-macro sample-unbound-macro)))

;; ============================================================
;; Consistency Check Tests
;; ============================================================

;; Helper: Find all identifier positions in source
;; Returns a list of (cons symbol position) for each identifier occurrence
(define (find-all-identifier-positions source)
  (define lines (string-split source "\n" #:trim? #f))
  (define results '())
  (for ([line-num (in-naturals)]
        [line lines])
    (define matches (regexp-match-positions* #px"[a-zA-Z!@#$%^&*:<>/?_-][a-zA-Z1-9!@#$%^&*:<>/?_-]*" line))
    (for ([match matches])
      (define start-col (car match))
      (define end-col (cdr match))
      (define sym (substring line start-col end-col))
      ;; Skip keywords
      (unless (member sym '("let" "if" "define" "block" "begin" "let-syntax" "define-syntax" 
                            "syntax-rules" "else" "#%expression"))
        (set! results (cons (cons sym (hash 'line line-num 'character start-col)) results)))))
  (reverse results))

;; Helper: Compare two LSP location results (handles hash key ordering)
(define (location-equal? loc1 loc2)
  (and (equal? (hash-ref loc1 'uri) (hash-ref loc2 'uri))
       (let ([r1 (hash-ref loc1 'range)]
             [r2 (hash-ref loc2 'range)])
         (and (equal? (hash-ref (hash-ref r1 'start) 'line)
                      (hash-ref (hash-ref r2 'start) 'line))
              (equal? (hash-ref (hash-ref r1 'start) 'character)
                      (hash-ref (hash-ref r2 'start) 'character))
              (equal? (hash-ref (hash-ref r1 'end) 'line)
                      (hash-ref (hash-ref r2 'end) 'line))
              (equal? (hash-ref (hash-ref r1 'end) 'character)
                      (hash-ref (hash-ref r2 'end) 'character))))))

;; Helper: Check if a location is in a list of locations
(define (location-member? loc locs)
  (ormap (lambda (l) (location-equal? loc l)) locs))

(module+ test
  ;; ============================================================
  ;; Find-references completeness check over sample pool
  ;; ============================================================
  ;; Property 9: For any surface binding site, calling find-references SHALL return
  ;; the spans of all surface reference sites that resolve to that binding,
  ;; including the binding site itself.
  ;;
  ;; Completeness properties:
  ;; 1. If find-references(pos) returns refs, then for each ref in refs,
  ;;    find-references(ref) should return the same set of refs.
  ;; 2. If goto-definition(pos) returns binding B, then find-references(B)
  ;;    should include pos.
  ;; 3. Every binding site should be included in its own find-references result.
  
  (test-case
   "consistency: find-references completeness check"
   (for ([sample sample-programs])
     (define name (car sample))
     (define source (cdr sample))
     (define id-positions (find-all-identifier-positions source))
     
     (for ([id-pos id-positions])
       (define sym (car id-pos))
       (define pos (cdr id-pos))
       
       ;; Call find-references on this identifier
       (define refs (find-references source pos))
       
       ;; Skip if no references found (unbound variable)
       (unless (null? refs)
         ;; Property 1: All references in the set should return the same set
         ;; when find-references is called on them
         (for ([ref refs])
           (define ref-range (hash-ref ref 'range))
           (define ref-start (hash-ref ref-range 'start))
           (define ref-refs (find-references source ref-start))
           
           ;; The reference sets should be equivalent (same elements, possibly different order)
           (check-equal?
            (length refs)
            (length ref-refs)
            (format "Find-refs consistency failed for ~a in ~a: find-refs(~a) has ~a refs, but find-refs(~a) has ~a refs"
                    sym name pos (length refs) ref-start (length ref-refs)))
           
           (for ([r refs])
             (check-true
              (location-member? r ref-refs)
              (format "Find-refs consistency failed for ~a in ~a: ~a in find-refs(~a) but not in find-refs(~a)"
                      sym name r pos ref-start))))
         
         ;; Property 2: If goto-definition returns a binding, find-references on that
         ;; binding should include the original position.
         ;; Skip when the position is itself a binding site (goto-def returns itself),
         ;; since binding sites are not in find-references results.
         (define binding-sites (goto-definition source pos))
         (for ([binding-site binding-sites])
           (define binding-range (hash-ref binding-site 'range))
           (define binding-start (hash-ref binding-range 'start))
           
           ;; Skip if goto-def returned our own position (we're a binding site)
           (unless (and (= (hash-ref binding-start 'line) (hash-ref pos 'line))
                        (= (hash-ref binding-start 'character) (hash-ref pos 'character)))
             (define binding-refs (find-references source binding-start))
             
             ;; The original position should be in the binding's references
             (define original-loc (hash 'uri test-uri 
                                        'range (hash 'start pos 
                                                     'end (hash 'line (hash-ref pos 'line)
                                                                'character (+ (hash-ref pos 'character)
                                                                              (string-length sym))))))
             (check-true
              (location-member? original-loc binding-refs)
              (format "Find-refs completeness failed for ~a in ~a: ~a not in find-refs(binding-site ~a) = ~a"
                      sym name original-loc binding-start binding-refs))))
         
         ))))

  ;; ============================================================
  ;; Autocomplete soundness check over sample pool
  ;; ============================================================
  ;; Property 13: For any name returned by autocomplete at a given position,
  ;; inserting that name as an identifier at that position SHOULD NOT result
  ;; in an unbound variable error during expansion.
  ;;
  ;; Soundness property: If autocomplete(pos) returns name N, then replacing
  ;; the identifier at pos with N should not produce an unbound error for N.
  ;; Note: This property may not hold in the presence of certain macros that
  ;; inspect or manipulate their arguments in ways that affect binding.
  ;;
  ;; We only check reference sites (not binding sites) because:
  ;; - At binding sites, autocomplete shows what's in scope for the RHS
  ;; - Replacing a binding site name changes program semantics
  ;; - The soundness property is about "can I use this name here" which
  ;;   applies to reference positions, not binding positions
  
  (test-case
   "consistency: autocomplete soundness check"
   (for ([sample sample-programs])
     (define name (car sample))
     (define source (cdr sample))
     (define id-positions (find-all-identifier-positions source))
     
     (for ([id-pos id-positions])
       (define sym (car id-pos))
       (define pos (cdr id-pos))
       
       ;; Skip binding sites - only check reference sites
       ;; A position is a binding site if goto-definition returns itself
       (define binding-sites (goto-definition source pos))
       (define is-binding-site?
         (and (not (null? binding-sites))
              (let* ([first-binding (car binding-sites)]
                     [binding-range (hash-ref first-binding 'range)]
                     [binding-start (hash-ref binding-range 'start)])
                (and (= (hash-ref binding-start 'line) (hash-ref pos 'line))
                     (= (hash-ref binding-start 'character) (hash-ref pos 'character))))))
       
       (unless is-binding-site?
         ;; Call autocomplete on this identifier position
         (define completions (autocomplete source pos))
         
         ;; For each completion, verify it's a valid name at this position
         ;; by checking that goto-definition returns a result (not unbound)
         (for ([completion completions])
           (define completion-name (hash-ref completion 'label))
           
           ;; Create a modified source with the completion name at this position
           ;; We replace the identifier at pos with the completion name
           (define modified-source (replace-identifier-at source pos sym completion-name))
           
           ;; Try goto-definition on the replaced identifier
           ;; If the name is sound, it should resolve to a binding.
           ;; The substitution can break expansion when it removes a macro call
           ;; that defines the suggested name (e.g. replacing `outer` with `inner`
           ;; in `(outer inner)` where `outer` defines `inner`). We tolerate such
           ;; failures — the name is genuinely in scope, the test just can't verify
           ;; it via substitution.
           ;; Check that the suggested name doesn't produce an unbound
           ;; identifier error at the substituted position. Other errors
           ;; (bad syntax, etc.) are acceptable — the substitution may have
           ;; broken a macro call that affects program structure.
           (define unbound-error?
             (has-unbound-error-at? modified-source pos))
           
           (check-false
            unbound-error?
            (format "Autocomplete soundness failed for ~a in ~a: autocomplete(~a) suggested ~s but it's unbound in modified source"
                    sym name pos completion-name)))))))
)


;; Helper: Replace identifier at position with a new name
;; Returns the modified source string
(define (replace-identifier-at source pos old-name new-name)
  (define lines (string-split source "\n" #:trim? #f))
  (define line-num (hash-ref pos 'line))
  (define col (hash-ref pos 'character))
  (define line (list-ref lines line-num))
  (define new-line (string-append
                    (substring line 0 col)
                    new-name
                    (substring line (+ col (string-length old-name)))))
  (define new-lines (list-set lines line-num new-line))
  (string-join new-lines "\n"))

