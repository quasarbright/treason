# DRY: Don't Repeat Yourself

## Before Writing New Code

1. Check whether the logic you need already exists in the codebase — a utility function, a method on a class, a shared module, etc.
2. If it exists, use it. Don't re-implement it.
3. If it almost exists, consider extending or generalizing the existing code rather than duplicating and tweaking it.

## When You Spot Duplication

- If two or more places share the same logic (or near-identical logic with minor variations), extract a shared abstraction: a function, a base class, a mixin, a higher-order function, a template — whatever fits the language and context.
- Parameterize the differences rather than copy-paste-modify.

## Use Judgment

- DRY is about knowledge duplication, not textual coincidence. Two blocks of code that happen to look similar but represent genuinely different concepts don't need to be merged.
- Don't over-abstract. If an abstraction makes the code harder to read or introduces coupling between unrelated modules, a small amount of repetition is fine.
- The rule of three is a reasonable heuristic: if you're about to write the same pattern a third time, it's time to extract.
