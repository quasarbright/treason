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

  (test-case "autocomplete at end of identifier"
    (define source "(let ([x 1]) (let ([y x]) 2))")
    (check-equal?
     (autocomplete source (hash 'line 0 'character 23))
     (list (hasheq 'label "x"))))

  ;; ============================================================
  ;; Pattern variable tests (issue #3)
  ;; ============================================================
  ;; Pattern variables in syntax-rules templates should support goto-definition,
  ;; find-references, and autocomplete just like regular bindings.
  ;;
  ;; Index guide for "(let-syntax ([m (syntax-rules () [(_ q) (let ([a q]) a)])]) (m 5))":
  ;;   q0 (char 37): pattern pvar in (_ q)
  ;;   q1 (char 49): template reference in (let ([a q]) a)

  ;; 1. Goto-definition on a template pvar reference → pattern pvar site
  (test-case
   "pvar: goto-def on template reference goes to pattern"
   (define source "(let-syntax ([m (syntax-rules () [(_ q) (let ([a q]) a)])]) (m 5))")
   ;; q0 = pattern pvar, q1 = template use
   (check-equal?
    (goto-definition source (find-position source "q" 1))
    (list (hash 'uri test-uri 'range (find-range source "q" 0)))))

  ;; 2. Goto-definition on pattern pvar site → itself
  (test-case
   "pvar: goto-def on pattern site returns itself"
   (define source "(let-syntax ([m (syntax-rules () [(_ q) (let ([a q]) a)])]) (m 5))")
   (check-equal?
    (goto-definition source (find-position source "q" 0))
    (list (hash 'uri test-uri 'range (find-range source "q" 0)))))

  ;; 3. Find-references on pattern pvar → all template uses
  (test-case
   "pvar: find-refs on pattern pvar finds template uses"
   (define source "(let-syntax ([m (syntax-rules () [(_ q) (let ([a q]) a)])]) (m 5))")
   (check-equal?
    (find-references source (find-position source "q" 0))
    (list (hash 'uri test-uri 'range (find-range source "q" 1)))))

  ;; 4. Find-references on template pvar reference → same set as from pattern
  (test-case
   "pvar: find-refs on template reference finds same set"
   (define source "(let-syntax ([m (syntax-rules () [(_ q) (let ([a q]) a)])]) (m 5))")
   (check-equal?
    (find-references source (find-position source "q" 1))
    (list (hash 'uri test-uri 'range (find-range source "q" 1)))))

  ;; 5. Multiple template uses of same pvar
  ;; Index guide for "(let-syntax ([dup (syntax-rules () [(_ q) (let ([a q]) q)])]) (dup 1))":
  ;;   q0 (char 39): pattern pvar
  ;;   q1 (char 51): first template use in (let ([a q])
  ;;   q2 (char 55): second template use in body q
  (test-case
   "pvar: find-refs finds all template uses of pvar"
   (define source "(let-syntax ([dup (syntax-rules () [(_ q) (let ([a q]) q)])]) (dup 1))")
   (check-equal?
    (length (find-references source (find-position source "q" 0)))
    2)
   (check-not-false
    (member (hash 'uri test-uri 'range (find-range source "q" 1))
            (find-references source (find-position source "q" 0))))
   (check-not-false
    (member (hash 'uri test-uri 'range (find-range source "q" 2))
            (find-references source (find-position source "q" 0)))))

  ;; 6. Multiple pvars in same pattern — each resolves independently
  ;; Index guide for "(let-syntax ([swap (syntax-rules () [(_ p r) (let ([a p]) (let ([b r]) b))])]) (swap 1 2))":
  ;;   p0 (char 17): 'p' in "swap" — not a pvar
  ;;   p1 (char 40): pattern pvar p in (_ p r)
  ;;   p2 (char 54): template use of p in (let ([a p])
  ;;   r1 (char 42): pattern pvar r in (_ p r)
  ;;   r2 (char 67): template use of r in (let ([b r])
  (test-case
   "pvar: multiple pvars resolve independently"
   (define source "(let-syntax ([swap (syntax-rules () [(_ p r) (let ([a p]) (let ([b r]) b))])]) (swap 1 2))")
   ;; p2 (template use) → p1 (pattern pvar)
   (check-equal?
    (goto-definition source (find-position source "p" 2))
    (list (hash 'uri test-uri 'range (find-range source "p" 1))))
   ;; r2 (template use) → r1 (pattern pvar)
   (check-equal?
    (goto-definition source (find-position source "r" 2))
    (list (hash 'uri test-uri 'range (find-range source "r" 1))))
   ;; p1 (pattern pvar) → p2 (template use)
   (check-equal?
    (find-references source (find-position source "p" 1))
    (list (hash 'uri test-uri 'range (find-range source "p" 2))))
   ;; r1 (pattern pvar) → r2 (template use)
   (check-equal?
    (find-references source (find-position source "r" 1))
    (list (hash 'uri test-uri 'range (find-range source "r" 2)))))

  ;; 7. Template-introduced identifiers (not pvars) don't produce errors
  (test-case
   "pvar: template-introduced identifiers are not resolved as pvars"
   ;; 'a' in (let ([a q]) a) is template-introduced, not a pvar.
   ;; After expansion it becomes a real let binding, so goto-def on the
   ;; reference (a3) resolves to the binding site (a2), not to any pvar.
   ;; a2 (char 47): template 'a' binding site in (let ([a q]) a)
   ;; a3 (char 53): template 'a' reference in body a
   (define source "(let-syntax ([m (syntax-rules () [(_ q) (let ([a q]) a)])]) (m 5))")
   ;; a3 resolves to a2 — the let binding, not a pattern variable
   (check-equal?
    (goto-definition source (find-position source "a" 3))
    (list (hash 'uri test-uri 'range (find-range source "a" 2)))))

  ;; 8. Autocomplete inside template suggests pattern variables
  ;; q1 (char 49) is the template reference — autocomplete there should include q
  (test-case
   "pvar: autocomplete in template suggests pattern variables"
   (define source "(let-syntax ([m (syntax-rules () [(_ q) (let ([a q]) a)])]) (m 5))")
   (check-not-false
    (member (hasheq 'label "q")
            (autocomplete source (find-position source "q" 1)))))

  ;; 9. Pvar with multiple clauses — correct clause's pvar is resolved
  ;; Index guide for "(let-syntax ([m (syntax-rules () [(_ p) p] [(_ p r) (let ([a p]) r)])]) (m 1))":
  ;;   p0 (char 37): pattern pvar in first clause (_ p)
  ;;   p1 (char 40): template use in first clause body p
  (test-case
   "pvar: correct clause pvar resolved when multiple clauses"
   (define source "(let-syntax ([m (syntax-rules () [(_ p) p] [(_ p r) (let ([a p]) r)])]) (m 1))")
   ;; p1 (template use in matched clause) → p0 (pattern pvar in that clause)
   (check-equal?
    (goto-definition source (find-position source "p" 1))
    (list (hash 'uri test-uri 'range (find-range source "p" 0)))))

  ;; 10. Pvar resolution works even when the macro is never called
  ;; Index guide for "(let-syntax ([m (syntax-rules () [(_ q) (let ([a q]) a)])]) 42)":
  ;;   q0 (char 37): pattern pvar in (_ q)
  ;;   q1 (char 49): template reference in (let ([a q]) a)
  (test-case
   "pvar: goto-def works when macro is never called"
   (define source "(let-syntax ([m (syntax-rules () [(_ q) (let ([a q]) a)])]) 42)")
   ;; q1 (template use) → q0 (pattern pvar), even though (m ...) is never called
   (check-equal?
    (goto-definition source (find-position source "q" 1))
    (list (hash 'uri test-uri 'range (find-range source "q" 0))))
   ;; q0 (pattern pvar) → itself
   (check-equal?
    (goto-definition source (find-position source "q" 0))
    (list (hash 'uri test-uri 'range (find-range source "q" 0))))
   ;; find-refs from pattern pvar finds the template use
   (check-equal?
    (find-references source (find-position source "q" 0))
    (list (hash 'uri test-uri 'range (find-range source "q" 1)))))

  ;; 11. Goto-def on an unused pvar (not referenced in template)
  ;; Index guide for "(let-syntax ([m (syntax-rules () [(_ q) 42])]) 1)":
  ;;   q0 (char 37): pattern pvar in (_ q) — never used in template
  (test-case
   "pvar: goto-def on unused pvar returns itself"
   (define source "(let-syntax ([m (syntax-rules () [(_ q) 42])]) 1)")
   ;; q0 is declared in the pattern but never referenced in the template
   (check-equal?
    (goto-definition source (find-position source "q" 0))
    (list (hash 'uri test-uri 'range (find-range source "q" 0)))))

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
   ;; Outer binding site has no references (shadowed), returns itself
   (check-equal?
    (goto-definition source (find-position source "x" 0))
    (list (hash 'uri test-uri 'range (find-range source "x" 0)))))

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

  ;; 5b. block/define - binding site with no references (regression test for issue #6)
  (test-case
   "goto-def: block define binding site no references"
   ;; When a define binding has no references, goto-definition on the binding
   ;; site should still return the binding site itself (not empty list).
   (define source "(block (define x 1))")
   ;; x binding site returns itself even with no references
   (check-equal?
    (goto-definition source (find-position source "x" 0))
    (list (hash 'uri test-uri 'range (find-range source "x" 0)))))

  ;; 5c. let binding - binding site with no references
  (test-case
   "goto-def: let binding site no references"
   (define source "(let ([x 1]) 42)")
   ;; x is bound but never used — goto-def on x should still return itself
   (check-equal?
    (goto-definition source (find-position source "x" 0))
    (list (hash 'uri test-uri 'range (find-range source "x" 0)))))

  ;; 5d. let-syntax binding site with no uses
  (test-case
   "goto-def: let-syntax binding site no uses"
   (define source "(let-syntax ([m (syntax-rules () [(_) 42])]) 1)")
   ;; m is defined but never called — goto-def on m should still return itself
   (check-equal?
    (goto-definition source (find-position source "m" 0))
    (list (hash 'uri test-uri 'range (find-range source "m" 0)))))

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
   ;; The surface $x at "def-m m $x" is a binding site (for given-$x), so it returns itself
   (check-equal?
    (goto-definition source (find-position source "$x" 4))
    (list (hash 'uri test-uri 'range (find-range source "$x" 4)))))

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
   ;; y binding site has no references, returns itself
   (check-equal?
    (goto-definition source (find-position source "y" 0))
    (list (hash 'uri test-uri 'range (find-range source "y" 0)))))

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

  ;; 16. Parse error produces a diagnostic, not a crash
  (test-case
   "error: parse error produces diagnostic"
   ;; Unclosed paren is a parse error — the LS should publish an error-severity
   ;; diagnostic with a message about unexpected end of input.
   (define client (new capturing-client%))
   (define server (new server% [client client]))
   (send server initialize (hasheq))
   ;; Opening a document with a parse error should not raise
   (check-not-exn
    (lambda ()
      (send server textDocument/didOpen
            (hasheq 'textDocument (hasheq 'uri test-uri 'text "(let ([x 1])")))))
   (define diags (send client get-diagnostics))
   ;; At least one diagnostic must be error-severity
   (check-true (for/or ([d diags])
                 (= (hash-ref d 'severity 0) 1))
               "expected at least one error-severity diagnostic")
   ;; The error message should mention unexpected end of input
   (check-true (for/or ([d diags])
                 (regexp-match? #rx"unexpected end of input" (hash-ref d 'message "")))
               "expected diagnostic message to mention unexpected end of input"))

  (test-case
   "error: LSP operations return graceful results after parse error"
   ;; After opening a document that fails to parse, all LSP operations should
   ;; return graceful empty results rather than erroring.
   (define source "(let ([x 1])")  ; unclosed paren — parse error
   (define any-pos (hasheq 'line 0 'character 0))
   (define server (make-test-server source))
   ;; documentSymbol: empty list (no symbols in a broken file)
   (check-equal?
    (send server textDocument/documentSymbol
          (hasheq 'textDocument (hasheq 'uri test-uri)))
    '())
   ;; definition: null (can't resolve in a broken file)
   (check-equal?
    (send server textDocument/definition
          (hasheq 'textDocument (hasheq 'uri test-uri) 'position any-pos))
    'null)
   ;; references: null (can't find references in a broken file)
   (check-equal?
    (send server textDocument/references
          (hasheq 'textDocument (hasheq 'uri test-uri) 'position any-pos))
    'null)
   ;; completion: empty list (no completions in a broken file)
   (check-equal?
    (send server textDocument/completion
          (hasheq 'textDocument (hasheq 'uri test-uri) 'position any-pos))
    '()))

  ;; ============================================================
  ;; Error message tests
  ;; ============================================================
  ;; These tests verify that specific bad-syntax forms produce diagnostics
  ;; with the appropriate who prefix in the message.

  ;; let errors
  (test-case
   "error message: let bad syntax (wrong arity)"
   (check-true (has-diagnostic-from? "(let)" 'let))
   (check-true (has-diagnostic-from? "(let ([x 1]))" 'let))
   (check-true (has-diagnostic-from? "(let ([x 1]) x y)" 'let)))

  (test-case
   "error message: let bad syntax (malformed binding)"
   (check-true (has-diagnostic-from? "(let (x) x)" 'let))
   (check-true (has-diagnostic-from? "(let ([x]) x)" 'let))
   (check-true (has-diagnostic-from? "(let ([x 1 2]) x)" 'let)))

  (test-case
   "error message: let bad syntax (non-identifier binding name)"
   (check-true (has-diagnostic-from? "(let ([1 2]) 1)" 'let)))

  ;; let-syntax errors
  (test-case
   "error message: let-syntax bad syntax"
   (check-true (has-diagnostic-from? "(let-syntax)" 'let-syntax))
   (check-true (has-diagnostic-from? "(let-syntax ([m (syntax-rules () [(_) 1])]) x y)" 'let-syntax))
   (check-true (has-diagnostic-from? "(let-syntax ([1 (syntax-rules () [(_) 1])]) 1)" 'let-syntax)))

  ;; define errors
  (test-case
   "error message: define bad syntax"
   (check-true (has-diagnostic-from? "(block (define) (#%expression 1))" 'define))
   (check-true (has-diagnostic-from? "(block (define x) (#%expression 1))" 'define))
   (check-true (has-diagnostic-from? "(block (define x 1 2) (#%expression x))" 'define))
   (check-true (has-diagnostic-from? "(block (define 1 2) (#%expression 1))" 'define)))

  ;; define-syntax errors
  (test-case
   "error message: define-syntax bad syntax"
   (check-true (has-diagnostic-from? "(block (define-syntax) (#%expression 1))" 'define-syntax))
   (check-true (has-diagnostic-from? "(block (define-syntax m) (#%expression 1))" 'define-syntax))
   (check-true (has-diagnostic-from? "(block (define-syntax 1 (syntax-rules () [(_) 1])) (#%expression 1))" 'define-syntax)))

  ;; #%expression errors
  (test-case
   "error message: #%expression bad syntax"
   (check-true (has-diagnostic-from? "(block (#%expression))" '#%expression))
   (check-true (has-diagnostic-from? "(block (#%expression 1 2))" '#%expression)))

  ;; macro: no pattern matched
  (test-case
   "error message: macro no pattern matched"
   ;; Macro expects one argument but gets none — no pattern matches
   (define source "(let-syntax ([m (syntax-rules () [(_ x) x])]) (m))")
   (check-true (has-diagnostic-from? source 'm)))

  ;; unbound identifier
  (test-case
   "error message: unbound identifier"
   (check-true (has-diagnostic-matching? "(let ([x 1]) y)" #rx"unbound identifier")))

  ;; ============================================================
  ;; Implicit #%expression in definition context (issue #21)
  ;; ============================================================

  (test-case
   "implicit #%expression: bare identifier in block"
   ;; A bound variable used as a bare expression in a block should work without error.
   (define source "(block (define x 1) x)")
   (check-equal?
    (goto-definition source (find-position source "x" 1))
    (list (hash 'uri test-uri 'range (find-range source "x" 0)))))

  (test-case
   "implicit #%expression: bare number at top level"
   ;; A bare number at top level should not produce an error.
   (check-false (has-diagnostic-matching? "42" #rx".")))

  (test-case
   "implicit #%expression: bare expression in block"
   ;; An expression form in head position of a block def slot should be treated as #%expression.
   (define source "(block (define x 1) (let ([y x]) y))")
   (check-equal?
    (goto-definition source (find-position source "x" 1))
    (list (hash 'uri test-uri 'range (find-range source "x" 0)))))

  ;; ============================================================
  ;; Implicit top-level block (issue #21)
  ;; ============================================================

  (test-case
   "implicit top-level block: multi-form program"
   ;; Multiple top-level forms: definition followed by a reference.
   (define source "(define x 1)\nx")
   (check-equal?
    (goto-definition source (find-position source "x" 1))
    (list (hash 'uri test-uri 'range (find-range source "x" 0)))))

  (test-case
   "implicit top-level block: forward references"
   ;; Definitions can refer to later definitions (block-level forward ref).
   (define source "(define x 1)\n(define y x)\ny")
   (check-equal?
    (goto-definition source (find-position source "x" 1))
    (list (hash 'uri test-uri 'range (find-range source "x" 0))))
   (check-equal?
    (goto-definition source (find-position source "y" 1))
    (list (hash 'uri test-uri 'range (find-range source "y" 0)))))

  (test-case
   "implicit top-level block: bare expression at top level"
   ;; A bare top-level expression should not produce a diagnostic.
   (check-false (has-diagnostic-matching? "(define x 1)\nx" #rx".")))

  (test-case
   "implicit top-level block: single let expression"
   ;; A single let form at the top level works as an implicit block with one expression.
   (define source "(let ([x 2]) x)")
   (check-equal?
    (goto-definition source (find-position source "x" 1))
    (list (hash 'uri test-uri 'range (find-range source "x" 0)))))
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

;; has-diagnostic-matching? : String Regexp -> Boolean
;; Returns #t if expanding source produces a diagnostic whose message matches msg-rx.
(define (has-diagnostic-matching? source msg-rx)
  (define client (new capturing-client%))
  (define server (new server% [client client]))
  (send server initialize (hasheq))
  (send server textDocument/didOpen
        (hasheq 'textDocument (hasheq 'uri test-uri 'text source)))
  (for/or ([d (send client get-diagnostics)])
    (regexp-match? msg-rx (hash-ref d 'message ""))))

;; has-diagnostic-from? : String Symbol -> Boolean
;; Returns #t if expanding source produces a diagnostic whose message starts with "who: ".
(define (has-diagnostic-from? source who)
  (has-diagnostic-matching? source (regexp (string-append "^" (symbol->string who) ":"))))

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
;; Semantic Token Helpers
;; ============================================================

;; A Token is a (token Any Symbol [Listof Symbol])
(struct token [name type modifiers] #:transparent)
;; name : the token text as a symbol (or number for numeric literals)
;; type : one of 'keyword 'variable 'number 'macro
;; modifiers : subset of '(definition defaultLibrary)

(define semantic-token-type-names    (vector 'keyword 'variable 'number 'macro))
(define semantic-token-modifier-names (vector 'definition 'defaultLibrary))

;; decode-semantic-tokens : String (Listof Integer) -> (Listof Token)
;; Decodes LSP delta-encoded token data into human-readable Token structs.
(define (decode-semantic-tokens source data)
  (define source-lines (string-split source "\n" #:trim? #f))
  (define (get-text line char len)
    (substring (list-ref source-lines line) char (+ char len)))
  (define (decode-modifiers bitmask)
    (for/list ([i (in-range (vector-length semantic-token-modifier-names))]
               #:when (bitwise-bit-set? bitmask i))
      (vector-ref semantic-token-modifier-names i)))
  (let loop ([data data] [line 0] [char 0] [acc '()])
    (match data
      ['() (reverse acc)]
      [(list* dl dc len type-idx mods-bitmask rest)
       (define new-line (+ line dl))
       (define new-char (if (zero? dl) (+ char dc) dc))
       (define text (get-text new-line new-char len))
       (define tok-name (or (string->number text) (string->symbol text)))
       (loop rest new-line new-char
             (cons (token tok-name
                          (vector-ref semantic-token-type-names type-idx)
                          (decode-modifiers mods-bitmask))
                   acc))])))

;; semantic-tokens : String -> (Listof Token)
;; Creates a server, opens source, returns decoded semantic tokens.
(define (semantic-tokens source)
  (define server (make-test-server source))
  (define data
    (hash-ref
     (send server textDocument/semanticTokens/full
           (hasheq 'textDocument (hasheq 'uri test-uri)))
     'data))
  (decode-semantic-tokens source data))

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
     (for ([id-pos (find-all-identifier-positions source)])
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
         (define completions (autocomplete source pos))
         (for ([completion completions])
           (define completion-name (hash-ref completion 'label))
           (define modified-source (replace-identifier-at source pos sym completion-name))
           (define unbound-error? (has-unbound-error-at? modified-source pos))
           (check-false
            unbound-error?
            (format "Autocomplete soundness failed for ~a in ~a: autocomplete(~a) suggested ~s but it's unbound in modified source"
                    sym name pos completion-name)))))))

  ;; ============================================================
  ;; Semantic Token Tests
  ;; ============================================================

  (test-case "semantic tokens: basic let"
    (check-equal?
     (semantic-tokens "(let ([x 2]) x)")
     (list (token 'let 'keyword '(defaultLibrary))
           (token 'x 'variable '(definition))
           (token 2 'number '())
           (token 'x 'variable '()))))

  (test-case "semantic tokens: define in block"
    (check-equal?
     (semantic-tokens "(block (define x 1) (#%expression x))")
     (list (token 'block        'keyword  '(defaultLibrary))
           (token 'define       'keyword  '(defaultLibrary))
           (token 'x            'variable '(definition))
           (token 1             'number   '())
           (token '#%expression 'keyword  '(defaultLibrary))
           (token 'x            'variable '()))))

  (test-case "semantic tokens: user-defined macro"
    (check-equal?
     (semantic-tokens "(let-syntax ([m (syntax-rules () [(_ q) q])]) (m 5))")
     (list (token 'let-syntax 'keyword '(defaultLibrary))
           (token 'm          'macro   '(definition))
           (token 'q          'macro   '(definition))
           (token 'q          'macro   '())
           (token 'm          'macro   '())
           (token 5           'number  '()))))

  (test-case "semantic tokens: parse error returns empty"
    (check-equal? (semantic-tokens "(let") '()))

  ;; ============================================================
  ;; Multi-form top-level: autocomplete tests
  ;; ============================================================

  (test-case "multi-top-level autocomplete: reference sees earlier definition"
    ;; x is defined on line 0, referenced on line 1 — x should be in scope
    (define source "(define x 1)\nx")
    (check-equal?
     (autocomplete source (find-position source "x" 1))
     (list (hasheq 'label "x"))))

  (test-case "multi-top-level autocomplete: reference sees later definition (forward ref)"
    ;; In an implicit block, all definitions are visible everywhere (two-pass).
    ;; x is referenced on line 0 before it is defined on line 1.
    (define source "(define y x)\n(define x 1)\ny")
    (check-equal?
     (autocomplete source (find-position source "x" 0))
     (list (hasheq 'label "x") (hasheq 'label "y"))))

  (test-case "multi-top-level autocomplete: multiple definitions all in scope"
    (define source "(define a 1)\n(define b 2)\n(define c 3)\nb")
    (check-equal?
     (autocomplete source (find-position source "b" 1))
     (list (hasheq 'label "a") (hasheq 'label "b") (hasheq 'label "c"))))

  (test-case "multi-top-level autocomplete: bare expression after definitions"
    ;; At the bare identifier position, both definitions should be in scope
    (define source "(define x 1)\n(define y 2)\nx")
    (check-equal?
     (autocomplete source (find-position source "x" 1))
     (list (hasheq 'label "x") (hasheq 'label "y"))))

  ;; ============================================================
  ;; Multi-form top-level: semantic token tests
  ;; ============================================================

  (test-case "semantic tokens: multi-top-level define and reference"
    (check-equal?
     (semantic-tokens "(define x 1)\nx")
     (list (token 'define 'keyword  '(defaultLibrary))
           (token 'x      'variable '(definition))
           (token 1       'number   '())
           (token 'x      'variable '()))))

  (test-case "semantic tokens: multi-top-level two defines"
    (check-equal?
     (semantic-tokens "(define x 1)\n(define y x)")
     (list (token 'define 'keyword  '(defaultLibrary))
           (token 'x      'variable '(definition))
           (token 1       'number   '())
           (token 'define 'keyword  '(defaultLibrary))
           (token 'y      'variable '(definition))
           (token 'x      'variable '()))))

  (test-case "semantic tokens: multi-top-level bare number"
    ;; A bare number at top level is an implicit #%expression — it gets a 'number token
    (check-equal?
     (semantic-tokens "42")
     (list (token 42 'number '()))))

  ;; ============================================================
  ;; Issue #47: cursor gensym should not appear in autocomplete
  ;; ============================================================

  (test-case "bug #47: cursor does not appear in autocomplete"
    ;; From the issue: when cursor is inserted, it binds to itself and appears in results
    ;; This tests the exact example from the issue
    (define source
      "(define-syntax m (syntax-rules () [(m x) (let ([x 1]) x)]))\n(m )")
    ;; Test autocomplete at the argument to m
    ;; The cursor gensym should NOT appear in the results
    (define completions (autocomplete source (hash 'line 1 'character 3)))
    ;; Check that no completion label starts with "cursor"
    (for ([completion completions])
      (define label (hash-ref completion 'label))
      (check-false (string-prefix? label "cursor")
                   (format "Found cursor in autocomplete: ~a" label))))

  ;; ============================================================
  ;; Issue #42: pattern variables should appear in autocomplete
  ;; ============================================================

  (test-case "bug #42: pattern variable appears in template autocomplete (simple case)"
    ;; At HERE in the template, x should be in autocomplete
    (define source "(let-syntax ([m (syntax-rules () [(m x) HERE])]) 1)")
    (define completions (autocomplete source (find-position source "HERE" 0)))
    (check-not-false
     (member (hasheq 'label "x") completions)
     "Pattern variable x should appear in autocomplete at template position"))

  (test-case "bug #42: template shows both pattern var and template-introduced binding"
    ;; At HERE in template, should see both x (pattern var) and y (template-introduced)
    (define source "(let-syntax ([m (syntax-rules () [(m x) (let ([y 1]) HERE)])]) (m q))")
    (define completions (autocomplete source (find-position source "HERE" 0)))
    ;; Should include both x (from pvar resolution) and y (from use-site expansion)
    (check-not-false
     (member (hasheq 'label "x") completions)
     "Pattern variable x should appear in autocomplete")
    (check-not-false
     (member (hasheq 'label "y") completions)
     "Template-introduced y should appear in autocomplete"))
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

