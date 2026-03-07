# GitHub Issues & Task Management

This project uses GitHub Issues + GitHub Projects for task management.
Repo: `quasarbright/treason`

## Quick Reference

```bash
gh issue list
gh issue view <number>
gh issue create --title "..." --body "..." --label "P1,task"
gh issue edit <number> --body "..."
gh issue comment <number> --body "..."
```

## Labels Convention

- Priority: `P1`, `P2`, `P3`
- Type: `task`, `bug`, `feature`, `epic`, `decision`, `chore`
- Domain: `expander`, `lsp`, `stx`, `error-handling`, etc.

## Sub-Issues

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

Include these sections when creating issues:

- `## Design` — technical approach
- `## Acceptance Criteria` — what "done" looks like
- `## Notes` — additional context or open questions
- `## Provenance` — links to parent/discovered-from issues (e.g., "Discovered from #42")

## Session Completion (Landing the Plane)

When ending a work session, complete ALL of these steps:

1. File issues for remaining work — `gh issue create --title "..." --body "..." --label "P2,task"`
2. Run quality gates (tests, linters, builds) if code changed
3. Commit with a descriptive message that closes the relevant issue:
   ```bash
   git add file1 file2
   git commit -m "fixed the table rendering bug" -m "fixes #12"
   ```
4. Hand off — provide context for the next session
