---
include: always
---

# Agent Instructions

This project uses GitHub Issues + GitHub Projects for task management.
Repo: quasarbright/treason

## Quick Reference

```bash
gh issue list                          # List open issues
gh issue view <number>                 # View issue details
gh issue create --title "..." --body "..." --label "P1,task"
gh issue close <number>               # Close an issue
gh issue edit <number> --body "..."   # Edit issue body
gh issue comment <number> --body "..."  # Add a comment
```

## Labels Convention

Priority: P1, P2, P3
Type: task, bug, feature, epic, decision, chore
Domain: expander, lsp, stx, error-handling, etc.

## Sub-Issues (Parent-Child)

```bash
gh sub-issue add <parent> --issue <child>
gh sub-issue list <parent>
```

## Blockers / Dependencies

```bash
gh api POST /repos/quasarbright/treason/issues/{N}/dependencies/blocked_by -f issue_id={M}
gh api /repos/quasarbright/treason/issues/{N}/dependencies
```

## Project Board

```bash
gh project item-list <project-number> --owner quasarbright
```

## Issue Body Sections

When creating issues, include these markdown sections as applicable:

- **## Design** — design notes and technical approach
- **## Acceptance Criteria** — what "done" looks like
- **## Notes** — additional context or open questions
- **## Provenance** — links to parent/discovered-from issues (e.g., "Discovered from #42")

## Landing the Plane (Session Completion)

**When ending a work session**, you MUST complete ALL steps below.

**MANDATORY WORKFLOW:**

1. **File issues for remaining work** — `gh issue create --title "..." --body "..." --label "P2,task"`
2. **Run quality gates** (if code changed) — Tests, linters, builds
3. **Update issue status** — `gh issue close <number>` for finished work
4. **Hand off** — Provide context for next session
