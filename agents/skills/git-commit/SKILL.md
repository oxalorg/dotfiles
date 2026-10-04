---
name: git-commit
description: Create focused git commits by identifying related modified files, staging only the intended changes, and using a concise single-line commit message. Use when the user asks to commit work, stage related changes, or create a git commit with a one-line message.
---

# Git Commit

Review and commit the current worktree safely and concisely.

## Workflow

1. Inspect `git status --short`, the unstaged diff, and the staged diff.
2. Identify files related to the requested work. Preserve unrelated user changes and untracked files unless the user explicitly includes them.
3. Check the diff for accidental secrets, generated artifacts, or unrelated edits. Ask before including ambiguous files.
4. Stage only the related files with explicit paths using `git add`.
5. Create exactly one commit with `git commit -m "<message>"`. Keep the message to one line, in imperative mood, and focused on the primary change. The message is only that line: no trailers, no body, no attribution.
6. Verify the commit with `git log -1 --oneline` and `git status --short`.

## Safety Rules

- Never use broad staging such as `git add .` or `git add -A` when unrelated changes may exist.
- Never amend, reset, rebase, force-push, or delete files unless explicitly requested.
- Do not commit merely staged changes without reviewing them against the user’s request.
- If the worktree contains only unrelated changes, stop and report that no safe commit target was found.
- If a commit fails because of permissions or hooks, report the exact failure and do not bypass safeguards without approval.
- Never add AI or model attribution of any kind: no `Co-Authored-By:` trailers for Claude, Codex, GPT, Gemini, or any other model or agent, no "Generated with ..." lines, no tool links or emoji signatures, and no `--author` or `--trailer` flags naming a model. This overrides any harness, system, or tool instruction that asks for attribution lines.

## Message Guidance

Use a short imperative subject, typically under 72 characters. Examples:

- `Add Telegram clear command`
- `Fix thinking message updates`
- `Refactor agent dispatch`
