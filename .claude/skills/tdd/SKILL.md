---
name: tdd
description: Test-driven workflow for any behavior change — bug fixes, new features, or modifications. Write a failing test first, then implement, then verify. Use when asked to fix a bug, add a feature, or change existing behavior.
---

# TDD: Test-Driven Behavior Change

Use this workflow any time behavior is being added or changed — bug fixes, new features, or modifications to existing logic.

## 1. Understand the Change

Clarify exactly what behavior is expected. Identify:
- The current behavior (or absence of behavior for new features)
- The desired behavior after the change
- Which function(s), module(s), or code paths are involved

Read the relevant code before writing anything.

## 2. Write a Failing Test

Before touching any production code, write a test that:
- Directly exercises the target behavior
- Fails with the current code (for bugs: reproduces the defect; for features: fails because the feature doesn't exist yet)
- Will pass once the change is correctly implemented

Run the tests and confirm the new test fails. If it already passes, your test isn't capturing the right thing — revisit step 1.

## 3. Implement the Change

Make the minimal change needed to satisfy the test. For bugs, fix the root cause. For features, implement the behavior. Avoid unrelated refactoring in the same change.

## 4. Verify

Run the full test suite. Confirm:
- Your new test now passes
- No previously passing tests have broken

If anything broke, fix it before moving on.

