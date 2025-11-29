# GitHub Copilot Instructions for clprompt

## Project Overview

**clprompt** is a Common Lisp implementation of Google's dotprompt format - an executable prompt template system for generative AI. The project follows rigorous CLOS-based object-oriented design principles.

**Key Features:**
- Handlebars-style template rendering
- YAML frontmatter configuration
- Schema validation for inputs/outputs
- Multiple LLM provider support (Gemini, OpenAI)

## Tech Stack

- **Language**: Common Lisp (SBCL)
- **Build System**: ASDF
- **Package Manager**: Quicklisp
- **Testing**: FiveAM
- **Dependencies**: cl-yaml, yason, dexador, cl-ppcre, alexandria, str

## Coding Guidelines

### Testing
- Always write tests for new features using FiveAM
- Run `(asdf:test-system :clprompt)` before committing
- Use `signals` for testing condition signaling

### Code Style
- Follow CLOS protocol-oriented design
- Define generic functions before implementing methods
- Use `:reader` over `:accessor` for immutable slots
- 80 character line limit (soft), 100 (hard)
- 2-space indentation

### Git Workflow
- Always commit `.beads/issues.jsonl` with code changes
- Run `bd sync` at end of work sessions

## Issue Tracking with bd

**CRITICAL**: This project uses **bd** for ALL task tracking. Do NOT create markdown TODO lists.

### Essential Commands

```bash
# Find work
bd ready --json                    # Unblocked issues

# Create and manage
bd create "Title" -t bug|feature|task -p 0-4 --json
bd update <id> --status in_progress --json
bd close <id> --reason "Done" --json

# Search
bd list --status open --priority 1 --json
bd show <id> --json

# Sync (CRITICAL at end of session!)
bd sync  # Force immediate export/commit/push
```

### Workflow

1. **Check ready work**: `bd ready --json`
2. **Claim task**: `bd update <id> --status in_progress`
3. **Work on it**: Implement, test, document
4. **Discover new work?** `bd create "Found bug" -p 1 --deps discovered-from:<parent-id> --json`
5. **Complete**: `bd close <id> --reason "Done" --json`
6. **Sync**: `bd sync` (flushes changes to git immediately)

### Priorities

- `0` - Critical (security, data loss, broken builds)
- `1` - High (major features, important bugs)
- `2` - Medium (default, nice-to-have)
- `3` - Low (polish, optimization)
- `4` - Backlog (future ideas)

## Project Structure

```
clprompt/
├── clprompt.asd          # ASDF system definition
├── src/
│   ├── package.lisp      # Package definition
│   ├── conditions.lisp   # Condition hierarchy
│   ├── protocol.lisp     # Generic function definitions (protocols)
│   ├── model.lisp        # Core domain classes
│   ├── parser.lisp       # .prompt file parsing
│   ├── template.lisp     # Handlebars template engine
│   ├── schema.lisp       # Schema validation
│   └── providers/        # LLM provider implementations
│       ├── protocol.lisp
│       ├── gemini.lisp
│       └── openai.lisp
├── tests/
│   └── *.lisp
├── examples/
│   └── *.prompt
└── .beads/
    ├── beads.db         # SQLite database (DO NOT COMMIT)
    └── issues.jsonl     # Git-synced issue storage
```

## Key Documentation

- **AGENTS.md** - Comprehensive AI agent guide (detailed workflows, advanced features)
- **CLAUDE.md** - Project guidelines and CLOS design principles
- **README.md** - User-facing documentation

## Important Rules

- Use bd for ALL task tracking
- Always use `--json` flag for programmatic use
- Run `bd sync` at end of sessions
- Do NOT create markdown TODO lists
- Do NOT commit `.beads/beads.db` (JSONL only)

---

**For detailed workflows and advanced features, see [AGENTS.md](../AGENTS.md)**
