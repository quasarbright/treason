# Requirements Document

## Introduction

This feature adds Language Server Protocol (LSP) support to the hygienic macro expander in `private/scope-graph-prototype.rkt`. The expander uses scope graphs (instead of scope sets) for hygiene. We want to track binding/reference relationships during expansion so we can answer LSP queries: goto definition, find references, and autocomplete.

The key insight is that only surface syntax nodes (from the original source) need tracking—macro-introduced nodes don't exist in the source and shouldn't be navigable. Surface nodes get integer IDs; macro-introduced nodes get `#f`.

## Glossary

- **Surface_Syntax**: Syntax nodes that originate from the user's source code, as opposed to macro-introduced syntax
- **Surface_Node_ID**: A unique integer identifier assigned to surface syntax nodes for LSP tracking
- **Expander**: The macro expansion system in `scope-graph-prototype.rkt` that transforms syntax using scope graphs
- **Scope_Graph**: The data structure used for hygienic name resolution, where scopes are vertices and parent relationships are edges
- **Binding_Site**: A surface syntax node where a name is defined (e.g., the `x` in `(let ([x 1]) ...)`)
- **Reference_Site**: A surface syntax node where a name is used (e.g., the `x` in `(let ([x 1]) x)`)
- **Definition_Table**: A multi-valued mapping from Surface_Node_IDs to their resolved binding information (the gensym'd name or macro-closure). Multi-valued because surface syntax can be duplicated by macros and bound multiple times.
- **Reference_Table**: A multi-valued mapping from Surface_Node_IDs of reference sites to the Surface_Node_ID of their binding site. Multi-valued because surface syntax can be duplicated by macros and resolved multiple times.
- **Scope_Table**: A multi-valued mapping from Surface_Node_IDs to the Scope (scope graph vertex) they were expanded under. Multi-valued because surface syntax can be duplicated by macros and expanded under different scopes. The LSP can traverse the scope graph to compute names in scope.
- **LSP_Server**: The component that handles LSP protocol requests and queries the expansion tables

## Requirements

### Requirement 1: Surface Syntax Node Identification

**User Story:** As a developer, I want surface syntax nodes to have unique identifiers, so that the LSP can track which source locations correspond to which bindings.

#### Acceptance Criteria

1. WHEN the Expander converts an s-expression to syntax, THE Expander SHALL assign a unique integer ID to each syntax node
2. WHEN a macro produces new syntax from its template, THE Expander SHALL assign `#f` as the ID for macro-introduced nodes
3. WHEN a macro substitutes a pattern variable with use-site syntax, THE Expander SHALL preserve the original Surface_Node_ID of that syntax
4. THE identifier struct SHALL contain fields for: the symbol name (`e`), the Surface_Node_ID (`id`), the source span (`span`), and the marks list (`marks`)

### Requirement 2: Definition Tracking

**User Story:** As a developer, I want the expander to record where names are defined, so that "goto definition" can find binding sites.

#### Acceptance Criteria

1. WHEN the Expander binds a name in a scope (via `let`, `define`, `let-syntax`, or `define-syntax`), THE Expander SHALL record the binding in the Definition_Table if the binding site has a Surface_Node_ID
2. WHEN recording a definition, THE Definition_Table SHALL map the binding site's Surface_Node_ID to the resolved binding (the gensym'd name or macro-closure)
3. WHEN a macro-introduced identifier is bound, THE Expander SHALL NOT record it in the Definition_Table (since it has no Surface_Node_ID)
4. WHEN surface syntax is duplicated by a macro and bound multiple times, THE Definition_Table SHALL store all bindings for that Surface_Node_ID
5. WHEN the Expander binds a name, THE Expander SHALL also record the binding site as a reference to itself in the Reference_Table (since binding sites are references for LSP purposes)

### Requirement 3: Reference Tracking

**User Story:** As a developer, I want the expander to record where names are referenced, so that "goto definition" and "find references" work correctly.

#### Acceptance Criteria

1. WHEN the Expander resolves an identifier to a binding, THE Expander SHALL record the reference in the Reference_Table if the reference site has a Surface_Node_ID and the binding site has a Surface_Node_ID
2. WHEN recording a reference, THE Reference_Table SHALL map the reference site's Surface_Node_ID to the binding site's Surface_Node_ID
3. WHEN a reference resolves to a macro-introduced binding (no Surface_Node_ID on binding site), THE Expander SHALL NOT record it in the Reference_Table
4. WHEN a macro-introduced identifier references a surface binding, THE Expander SHALL NOT record it (the reference site has no ID)
5. WHEN surface syntax is duplicated by a macro and resolved multiple times, THE Reference_Table SHALL store all resolutions for that Surface_Node_ID
6. WHEN the Expander binds a name with a Surface_Node_ID, THE Expander SHALL record the binding site as a reference to itself (binding sites are references for LSP purposes)

### Requirement 4: Scope Tracking for Autocomplete

**User Story:** As a developer, I want to know what names are in scope at any source location, so that autocomplete can suggest valid completions.

#### Acceptance Criteria

1. WHEN the Expander expands a surface syntax node, THE Expander SHALL record the scope graph vertex in the Scope_Table
2. WHEN recording scope information, THE Scope_Table SHALL map the Surface_Node_ID to the Scope vertex that was active during expansion
3. WHEN surface syntax is duplicated by a macro and expanded under different scopes, THE Scope_Table SHALL store all scope vertices for that Surface_Node_ID
4. THE LSP_Server SHALL compute names in scope by traversing the scope graph from the stored vertex

### Requirement 5: Goto Definition Query

**User Story:** As a developer, I want to jump from a reference to its definition, so that I can navigate my code efficiently.

#### Acceptance Criteria

1. WHEN the LSP_Server receives a goto-definition request for a position, THE LSP_Server SHALL find the surface syntax node at that position
2. WHEN a surface syntax node is found, THE LSP_Server SHALL look up its Surface_Node_ID in the Reference_Table to find the binding site's ID
3. WHEN a binding site ID is found, THE LSP_Server SHALL return the source span of that binding site
4. IF no binding is found for the position, THEN THE LSP_Server SHALL return an empty result

### Requirement 6: Find References Query

**User Story:** As a developer, I want to find all uses of a binding, so that I can understand how a name is used throughout my code.

#### Acceptance Criteria

1. WHEN the LSP_Server receives a find-references request for a position, THE LSP_Server SHALL find the surface syntax node at that position
2. WHEN a surface syntax node is found at a binding site, THE LSP_Server SHALL find all reference site IDs that point to that binding
3. WHEN a surface syntax node is found at a reference site, THE LSP_Server SHALL first find its binding site, then find all references to that binding
4. THE LSP_Server SHALL return the source spans of all reference sites (including the binding site itself)
5. IF no binding is found for the position, THEN THE LSP_Server SHALL return an empty result

### Requirement 7: Autocomplete Query

**User Story:** As a developer, I want to see what names I can use at a given position, so that I can write code faster with fewer errors.

#### Acceptance Criteria

1. WHEN the LSP_Server receives an autocomplete request for a position, THE LSP_Server SHALL find the surface syntax node at or near that position
2. WHEN a surface syntax node is found, THE LSP_Server SHALL look up its Surface_Node_ID in the Scope_Table to get the scope vertex
3. THE LSP_Server SHALL traverse the scope graph from the vertex to collect all accessible names
4. WHEN multiple scope vertices are stored for a Surface_Node_ID, THE LSP_Server SHALL intersect the names from all vertices (conservative approach)
5. IF no scope information is found, THEN THE LSP_Server SHALL return an empty result

### Requirement 8: Hygiene Preservation

**User Story:** As a developer, I want LSP features to respect macro hygiene, so that I only see bindings that are actually accessible from my code.

#### Acceptance Criteria

1. WHEN a macro introduces a binding that shadows a surface binding, THE Reference_Table SHALL correctly track that surface references still resolve to the surface binding (not the macro-introduced one)
2. WHEN a surface binding is captured by a macro (unhygienic capture), THE Reference_Table SHALL reflect the actual resolution behavior of the expander
3. THE Scope_Table SHALL only show names that are hygienically accessible from each surface syntax position
