# AGENTS.md - AI Agent Guidelines for clprompt

## Issue Tracking with bd (beads)

**IMPORTANT**: This project uses **bd (beads)** for ALL issue tracking. Do NOT use markdown TODOs, task lists, or other tracking methods.

### Why bd?

- Dependency-aware: Track blockers and relationships between issues
- Git-friendly: Auto-syncs to JSONL for version control
- Agent-optimized: JSON output, ready work detection, discovered-from links
- Prevents duplicate tracking systems and confusion

### Quick Start

**Check for ready work:**
```bash
bd ready --json
```

**Create new issues:**
```bash
bd create "Issue title" -t bug|feature|task -p 0-4 --json
bd create "Issue title" -p 1 --deps discovered-from:bd-123 --json
```

**Claim and update:**
```bash
bd update bd-42 --status in_progress --json
bd update bd-42 --priority 1 --json
```

**Complete work:**
```bash
bd close bd-42 --reason "Completed" --json
```

### Issue Types

- `bug` - Something broken
- `feature` - New functionality
- `task` - Work item (tests, docs, refactoring)
- `epic` - Large feature with subtasks
- `chore` - Maintenance (dependencies, tooling)

### Priorities

- `0` - Critical (security, data loss, broken builds)
- `1` - High (major features, important bugs)
- `2` - Medium (default, nice-to-have)
- `3` - Low (polish, optimization)
- `4` - Backlog (future ideas)

### Workflow for AI Agents

1. **Check ready work**: `bd ready` shows unblocked issues
2. **Claim your task**: `bd update <id> --status in_progress`
3. **Work on it**: Implement, test, document
4. **Discover new work?** Create linked issue:
   - `bd create "Found bug" -p 1 --deps discovered-from:<parent-id>`
5. **Complete**: `bd close <id> --reason "Done"`
6. **Commit together**: Always commit the `.beads/issues.jsonl` file together with the code changes so issue state stays in sync with code state

### Auto-Sync

bd automatically syncs with git:
- Exports to `.beads/issues.jsonl` after changes (5s debounce)
- Imports from JSONL when newer (e.g., after `git pull`)
- No manual export/import needed!

### GitHub Copilot Integration

If using GitHub Copilot, also create `.github/copilot-instructions.md` for automatic instruction loading.
Run `bd onboard` to get the content, or see step 2 of the onboard instructions.

### MCP Server (Recommended)

If using Claude or MCP-compatible clients, install the beads MCP server:

```bash
pip install beads-mcp
```

Add to MCP config (e.g., `~/.config/claude/config.json`):
```json
{
  "beads": {
    "command": "beads-mcp",
    "args": []
  }
}
```

Then use `mcp__beads__*` functions instead of CLI commands.

### Managing AI-Generated Planning Documents

AI assistants often create planning and design documents during development:
- PLAN.md, IMPLEMENTATION.md, ARCHITECTURE.md
- DESIGN.md, CODEBASE_SUMMARY.md, INTEGRATION_PLAN.md
- TESTING_GUIDE.md, TECHNICAL_DESIGN.md, and similar files

**Best Practice: Use a dedicated directory for these ephemeral files**

**Recommended approach:**
- Create a `history/` directory in the project root
- Store ALL AI-generated planning/design docs in `history/`
- Keep the repository root clean and focused on permanent project files
- Only access `history/` when explicitly asked to review past planning

**Example .gitignore entry (optional):**
```
# AI planning documents (ephemeral)
history/
```

**Benefits:**
- Clean repository root
- Clear separation between ephemeral and permanent documentation
- Easy to exclude from version control if desired
- Preserves planning history for archeological research
- Reduces noise when browsing the project

### Important Rules

- Use bd for ALL task tracking
- Always use `--json` flag for programmatic use
- Link discovered work with `discovered-from` dependencies
- Check `bd ready` before asking "what should I work on?"
- Store AI planning docs in `history/` directory
- Do NOT create markdown TODO lists
- Do NOT use external issue trackers
- Do NOT duplicate tracking systems
- Do NOT clutter repo root with planning documents

For more details, see README.md and QUICKSTART.md.

## Multi-Agent Coordination (MCP Agent Mail)

This project runs multiple coding agents in parallel (Claude Code, Codex CLI, Gemini CLI). Coordination happens via MCP Agent Mail to prevent conflicts and enable async communication.

### Setup

Install MCP Agent Mail (one-time):
```bash
curl -fsSL https://raw.githubusercontent.com/Dicklesworthstone/mcp_agent_mail/main/scripts/install.sh | bash -s -- --yes
```

### Before Starting Work

1. **Register your identity** (once per session):
   ```
   register_agent(
     project_key="/home/wal/projects/clprompt",
     program="<your-cli>",  # claude-code, codex-cli, gemini-cli
     model="<your-model>"
   )
   ```

2. **Check inbox for messages from other agents**:
   ```
   fetch_inbox(
     project_key="/home/wal/projects/clprompt",
     agent_name="<your-name>"
   )
   ```

3. **Reserve files before editing** (prevents conflicts):
   ```
   file_reservation_paths(
     project_key="/home/wal/projects/clprompt",
     agent_name="<your-name>",
     paths=["src/providers/**"],
     exclusive=true,
     reason="bd-42",
     ttl_seconds=3600
   )
   ```

4. **Announce your work** (thread_id = beads issue ID):
   ```
   send_message(
     project_key="/home/wal/projects/clprompt",
     sender_name="<your-name>",
     to=["all"],
     subject="[bd-42] Starting provider refactor",
     body_md="Claiming this task. Will be working on src/providers/.",
     thread_id="bd-42"
   )
   ```

### File Reservation Etiquette

- **Always reserve before editing** - especially `src/` files
- **Use beads issue ID as reason** - links reservations to tasks
- **Release when done**: `release_file_reservations(...)`
- **Check conflicts first**: if reservation fails with conflict, coordinate via messages
- **TTL**: default 1 hour, renew with `renew_file_reservations(...)` if needed

### Agent Roles

| Agent | CLI | Typical Tasks |
|-------|-----|---------------|
| Claude | `claude` | Architecture, complex refactors, orchestration |
| Codex | `codex` | Implementation, tests, straightforward features |
| Gemini | `gemini` | Documentation, research, alternative implementations |

### Coordination Rules

1. **One agent per beads issue** at a time
2. **Always claim issue** with `bd update <id> --status in_progress` before starting
3. **Message other agents** when:
   - You need input on a design decision
   - You're blocked by their work
   - You discovered work that affects their area
   - You're done and releasing files
4. **Check inbox** at session start and after long tasks
5. **Thread IDs = Beads issue IDs** - keeps everything linked

### Quick Reference

```bash
# Check who's working on what
resource://file_reservations/clprompt?active_only=true

# See all registered agents
resource://project/clprompt

# Search past messages
search_messages(project_key="/home/wal/projects/clprompt", query="provider")
```

---

## Session-ending protocol (Landing the Plane)

**When the user says "let's land the plane"**, you MUST complete ALL steps below. The plane is NOT landed until `git push` succeeds. NEVER stop before pushing. NEVER say "ready to push when you are!" - that is a FAILURE.

**MANDATORY WORKFLOW - COMPLETE ALL STEPS:**

1. **File beads issues for any remaining work** that needs follow-up

2. **Ensure all quality gates pass** (only if code changes were made) - run tests, linters, builds (file P0 issues if broken)

3. **Update beads issues** - close finished work, update status

4. **PUSH TO REMOTE - NON-NEGOTIABLE** - This step is MANDATORY. Execute ALL commands below:

```bash
# Pull first to catch any remote changes
git pull --rebase

# If conflicts in .beads/beads.jsonl, resolve thoughtfully:
#   - git checkout --theirs .beads/beads.jsonl (accept remote)
#   - bd import -i .beads/beads.jsonl (re-import)
#   - Or manual merge, then import

# Sync the database (exports to JSONL, commits)
bd sync

# MANDATORY: Push everything to remote
# DO NOT STOP BEFORE THIS COMMAND COMPLETES
git push

# MANDATORY: Verify push succeeded
git status  # MUST show "up to date with origin/main"
```

**CRITICAL RULES:**

 - The plane has NOT landed until git push completes successfully
 - NEVER stop before git push - that leaves work stranded locally
 - NEVER say "ready to push when you are!" - YOU must push, not the user
 - If git push fails, resolve the issue and retry until it succeeds
 - The user is managing multiple agents - unpushed work breaks their coordination workflow

5. **Clean up git state** - Clear old stashes and prune dead remote branches:

```bash
git stash clear                    # Remove old stashes
git remote prune origin            # Clean up deleted remote branches
```

6. **Verify clean state** - Ensure all changes are committed AND PUSHED, no untracked files remain

7. **Choose a follow-up issue for next session**

 - Provide a prompt for the user to give to you in the next session
 - Format: "Continue work on bd-X: [issue title]. [Brief context about what's been done and what's next]"

**REMEMBER: Landing the plane means EVERYTHING is pushed to remote. No exceptions. No "ready when you are". PUSH IT.**

**Example "land the plane" session:**

```bash
# 1. File remaining work
bd create "Add integration tests for sync" -t task -p 2 --json

# 2. Run quality gates (only if code changes were made)
go test -short ./...
golangci-lint run ./...

# 3. Close finished issues
bd close bd-42 bd-43 --reason "Completed" --json

# 4. PUSH TO REMOTE - MANDATORY, NO STOPPING BEFORE THIS IS DONE
git pull --rebase
# If conflicts in .beads/beads.jsonl, resolve thoughtfully:
#   - git checkout --theirs .beads/beads.jsonl (accept remote)
#   - bd import -i .beads/beads.jsonl (re-import)
#   - Or manual merge, then import
bd sync        # Export/import/commit
git push       # MANDATORY - THE PLANE IS STILL IN THE AIR UNTIL THIS SUCCEEDS
git status     # MUST verify "up to date with origin/main"

# 5. Clean up git state
git stash clear
git remote prune origin

# 6. Verify everything is clean and pushed
git status

# 7. Choose next work
bd ready --json
bd show bd-44 --json
```

**Then provide the user with:**

 - Summary of what was completed this session
 - What issues were filed for follow-up
 - Status of quality gates (all passing / issues filed)
 - Confirmation that ALL changes have been pushed to remote
 - Recommended prompt for next session

**CRITICAL: Never end a "land the plane" session without successfully pushing. The user is coordinating multiple agents and unpushed work causes severe rebase conflicts.**
