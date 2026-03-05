---
name: fix-bug
description: Guided workflow for fixing a bug with a regression test
---

# Bug Fix Workflow

Follow these steps in order when fixing a bug.

## 1. Understand the Bug

Read the bug report or reproduction steps carefully. Identify:
- What the incorrect behavior is
- What the expected behavior should be
- Which function(s) or module(s) are likely responsible

## 2. Write a Failing Test

Before touching any production code, write a test that:
- Directly exercises the buggy behavior
- Fails with the current (broken) code
- Will pass once the bug is fixed

Run the test suite to confirm the new test fails. If it passes already, you don't have a regression test — revisit your understanding of the bug.

This test serves two purposes: it proves you've reproduced the bug, and it becomes a permanent regression guard.

## 3. Fix the Bug

Make the minimal change needed to fix the bug. Avoid refactoring unrelated code in the same commit — keep the diff focused and reviewable.

## 4. Verify the Fix

Run the test suite again. Confirm:
- Your new test now passes
- No previously passing tests have broken

If any existing tests broke, investigate before proceeding — your fix may have introduced a regression.

## 5. Commit

Stage and commit with a descriptive message. Reference the issue number if one exists:

```
git add <changed files>
git commit -m "fix: <short description of the bug>" -m "fixes #<issue-number>"
```
