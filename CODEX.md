# CODEX.md - Codex CLI Instructions for clprompt

You are a Codex CLI agent working on a Common Lisp project.

**Read these files first:**
- `AGENTS.md` - Full workflow, beads commands, multi-agent coordination
- `CLAUDE.md` - Project structure, CLOS design principles, code style

## Your Identity

Register with MCP Agent Mail at session start:
```
register_agent(
  project_key="/home/wal/projects/clprompt",
  program="codex-cli",
  model="codex"
)
```

## Your Role

- **Primary**: Implementation, tests, straightforward features
- **Strengths**: Fast iteration, test coverage, focused tasks
- **Avoid**: Major architectural decisions (coordinate with Claude first)

## Session Start Checklist

1. **Check for messages**:
   ```
   fetch_inbox(project_key="/home/wal/projects/clprompt", agent_name="<your-name>")
   ```

2. **Find work**:
   ```bash
   bd ready --json
   ```

3. **Claim a task**:
   ```bash
   bd update bd-<id> --status in_progress --json
   ```

4. **Reserve files** you'll edit:
   ```
   file_reservation_paths(
     project_key="/home/wal/projects/clprompt",
     agent_name="<your-name>",
     paths=["src/schema.lisp", "tests/schema-tests.lisp"],
     exclusive=true,
     reason="bd-<id>",
     ttl_seconds=3600
   )
   ```

5. **Announce** in the thread:
   ```
   send_message(
     project_key="/home/wal/projects/clprompt",
     sender_name="<your-name>",
     to=["all"],
     subject="[bd-<id>] Starting work",
     body_md="Claiming this task. Reserved: src/schema.lisp, tests/schema-tests.lisp",
     thread_id="bd-<id>"
   )
   ```

## During Work

- Run tests frequently: `(asdf:test-system :clprompt)`
- If you discover new issues: `bd create "..." -p 2 --deps discovered-from:bd-<parent>`
- If blocked by another agent's files: send them a message, don't wait silently

## Session End

**Follow "Landing the Plane" protocol in AGENTS.md exactly.**

Key steps:
1. Close your beads issue: `bd close bd-<id> --reason "Completed"`
2. Release file reservations: `release_file_reservations(...)`
3. Message completion: `[bd-<id>] Completed - <summary>`
4. `bd sync && git push` - **MANDATORY, do not skip**

## Common Lisp Reminders

- Load system: `(ql:quickload :clprompt)`
- Run tests: `(asdf:test-system :clprompt)`
- Use FiveAM for testing
- Follow CLOS patterns in CLAUDE.md
