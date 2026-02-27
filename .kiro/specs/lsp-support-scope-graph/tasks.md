# Tasks: LSP Support for Scope Graph Expander

## Phase 1: Tests (in lsp-tests.rkt)

Tests are written against the LSP server interface. They are expected to mostly fail initially, since we're writing tests for the NEW expander which has different behavior from the existing one.

### 1.1 Sample Program Pool
- [x] Create a list of sample programs covering:
  - Simple let bindings
  - Nested scopes with shadowing  
  - Forward references in blocks (define)
  - let-syntax and define-syntax
  - Macro that duplicates use-site syntax
  - Macro that introduces bindings
  - Nested macro applications
  - Fig. 17 hygiene example
  - Programs with errors (unbound, bad syntax)

### 1.2 Targeted LSP Tests
- [x] Goto-definition tests for each binding form
- [x] Find-references tests (forward refs, multiple refs, self-refs)
- [x] Autocomplete tests at various positions
- [x] Shadowing tests (inner binding shadows outer)
- [x] Hygiene tests (surface refs resolve to surface bindings)
- [x] Error case tests (unbound still allows autocomplete)

### 1.3 Consistency Check Tests
- [x] Round-trip goto-definition check over sample pool
- [x] Find-references completeness check over sample pool
- [x] Autocomplete soundness check over sample pool
- [ ] Table consistency check over sample pool

## Phase 2: Data Structures

### 2.1 Update stx.rkt
- [x] Add `id` field to stx struct (Natural or #f)
- [x] Add `marks` field to stx struct (Listof Mark)
- [x] Update stx-e? contract if needed
- [x] Add identifier? predicate and accessors

### 2.2 Add LSP Types to expander.rkt (new scope-graph-based expander)
- [x] 2.2
    - [x] Add `identifier-key` struct
    - [x] Add `identifier->key` function
    - [x] Add `var-binding` struct (with site field)
    - [x] Add `keyword-binding` struct
    - [x] Add `macro-binding` struct (with site field)
    - [x] Add `resolution` struct
    - [x] Add `stx-error` struct
    - [x] Add `expander-state` struct
    - [x] Add `expander-result` struct
    - [x] Add `current-expander-state` parameter

## Phase 3: Table Recording

### 3.1 Recording Functions
- [x] Implement `record-stx!`
- [x] Implement `record-resolution!`
- [x] Implement `hash-cons!` helper

### 3.2 Integrate Recording into Expander
- [x] Update `scope-bind!` to use IdentifierKey
- [x] Update `scope-resolve` to call `record-resolution!`
- [x] Update PatternEnv to use IdentifierKey
- [x] Update `project-def` / `project-use` for new stx struct
- [x] Update `mark-syntax` to preserve id/span
- [x] Update `combine-projections` to preserve use-site id/span
- [x] Add `maybe-set-use-site-span` after macro expansion

### 3.3 Entry Point
- [x] Implement `analyze!` function
- [x] Implement `record-all-stx!` 
- [x] Implement `find-stx-errors`

## Phase 4: Query Functions

### 4.1 Core Query Functions
- [x] Implement `get-binding-sites-of`
- [x] Implement `get-reference-sites-of`
- [x] Implement `get-names-in-scope`
- [x] Implement `scope->names`
- [x] Implement `get-all-surface-binding-sites`
- [x] Implement `binding-site` helper

### 4.2 Position-Based Functions
- [x] Implement `find-node-at-position`
- [x] Implement `goto-definition`
- [x] Implement `find-references`
- [x] Implement `autocomplete`
- [x] Implement `make-cursor`
- [x] Implement `insert-cursor-at`

## Phase 5: Integration

### 5.1 Reader Updates
- [x] Update reader.rkt to assign IDs during parsing
- [x] Update reader.rkt to create stx with id/marks fields

### 5.2 Replace expander.rkt
- [x] Move scope-graph-prototype.rkt logic to expander.rkt (replacing old unhygienic expander)
- [x] Update server.rkt to work with new expander API
- [ ] Verify all lsp-tests.rkt tests pass

## Phase 6: Light Unit Tests (in expander.rkt)

- [ ] Basic expansion test with new stx struct
- [ ] Table population test for simple let
- [ ] Fig. 17 hygiene test with LSP verification
