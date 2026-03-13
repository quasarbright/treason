# Code Organization: How to Design Programs

Follow these conventions when writing or modifying code, inspired by *How to Design Programs*.

## Module Structure

Organize every file top-down in this order:

1. Imports / dependencies
2. Exports (if applicable)
3. Constants and configuration
4. Data definitions / type declarations
5. Main (entry-point) functions
6. Helper functions used by the main functions (ordered top-down — a caller appears before its callees)

## Signatures

Every function must have a signature.

- In statically typed languages, the type signature serves this role naturally.
- In dynamically typed languages, add a comment directly above the function:

```python
# calculate_total : List[LineItem], float -> float
```

```javascript
// calculateTotal : Array<LineItem>, number -> number
```

```ruby
# calculate_total : Array<LineItem>, Float -> Float
```

In Racket, use:

```racket
;; function-name : InputType -> OutputType
```

## Purpose Statements

Every function, class, and module must have a brief purpose statement using the idiomatic doc format for the language. In Racket, a comment directly above the function. A purpose statement is one or two sentences describing *what* the function does, not *how*.

## Data Definitions

Define and document your data before you use it.

- Group related type aliases, interfaces, structs, enums, or class definitions near the top of the file, after imports.
- Each data definition should have a brief comment explaining what it represents and any constraints (e.g. "a non-empty list", "a positive integer").

## Examples / Tests

When writing functions, consider including at least one inline example or unit test that illustrates the expected input-output behavior, especially for non-trivial logic.

## Verification Checklist

Before finishing any code change, explicitly verify each of the following:

1. **Ordering**: For every new or moved function, confirm all its callers appear *above* it in the file. A helper must never precede the function that calls it. Check this by scanning upward from each new function — if you see a caller defined below, swap the order.
2. **Signatures**: Every new function has a type signature comment immediately above it.
3. **Purpose statements**: Every new function has a one-to-two sentence purpose statement.

Do not skip this checklist. Ordering mistakes are easy to make when inserting helpers adjacent to existing functions — the natural "define before use" instinct from other languages is backwards here.
