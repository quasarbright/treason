---
include: always
---

# Agent Instructions

This project uses **bd** (beads) for issue tracking. Run `bd onboard` to get started.

## Quick Reference

```bash
bd ready              # Find available work (open, no blockers)
bd blocked            # Show blocked issues and what blocks them
bd list               # List all issues (with blocker annotations)
bd show <id>          # View issue details
bd claim <id>         # Claim work (atomic compare-and-swap)
bd close <id>         # Complete work
bd dolt push          # Push to Dolt remote
```

**Dependency status**: `bd ready` and `bd blocked` are the authoritative
sources for whether work is blocked. `bd list` shows active blocker
annotations but use `bd ready`/`bd blocked` for accurate blocking status.

## Agent Warning: Interactive Commands

**DO NOT use `bd edit`** - it opens an interactive editor ($EDITOR) which AI agents cannot use.

Use `bd update` with flags instead:
```bash
bd update <id> --description "new description"
bd update <id> --title "new title"
bd update <id> --design "design notes"
bd update <id> --notes "additional notes"
bd update <id> --acceptance "acceptance criteria"
```

## Landing the Plane (Session Completion)

**When ending a work session**, you MUST complete ALL steps below.

**MANDATORY WORKFLOW:**

1. **File issues for remaining work** - Create issues for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **Hand off** - Provide context for next session
