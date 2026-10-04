# Codex conversations in tmux snapshots

Linux integration for tmux-resurrect and tmux-continuum. Each foreground Codex
TUI is saved as an exact `codex resume <UUID>` command, with its workspace,
CODEX_HOME and supported explicit launch options. Conversations in the same
directory remain distinct. No API calls are made by the bridge.

## Install

With tmux-resurrect already installed:

```sh
python3 ~/projects/dotfiles/tmux/codex-resurrect/install.py
tmux source-file ~/.tmux.conf
```

The installer links this directory into `~/.tmux/plugins/codex-resurrect`, links
the launcher into `~/.local/bin/codex-tmux-resume`, and merges a SessionStart
handler into `~/.codex/hooks.json`. Existing hooks are preserved. The tmux config
loads `load.sh` before TPM; this registers the post-save-layout hook and adds
the launcher to Resurrect's allowed processes without enabling arbitrary
process restoration. An existing different post-save-layout hook is reported
instead of replaced; it must be explicitly chained to this integration.

Review/trust the new handler in Codex with `/hooks`. Existing Codex processes
need not restart: the fallback reads metadata from their open rollout files.
Custom CODEX_HOME locations need the same hook in their own hooks.json if you
want lifecycle tracking there; the fallback also works with those locations.

The installer also patches Resurrect's shared scrollback filename helper to
encode `%` as `%25` and `/` as `%2F`. Both save and restore use the same helper;
session names remain unchanged and encoded names cannot collide. The original
helper is backed up alongside it. This is a local upstream-plugin modification:
review/reapply it by rerunning the installer after updating Resurrect. Old
scrollback archives containing literal `%` names are not migrated; take a fresh
snapshot after installation. Previously failed slash-name captures cannot be
recovered from their old archives.

## How it works

1. A SessionStart hook writes a binding keyed by boot ID, process ID and process
   start time. This avoids reusing a binding after a process exits or PID reuse.
2. At each Resurrect save, the bridge finds foreground native `codex` processes
   descended from each pane. It checks open rollout metadata, ignoring subagents.
   A hook binding is preferred only if consistent with the open root sessions.
   Without a binding, exactly one open root conversation is required.
3. The bridge replaces each matching pane's command in the new snapshot with
   the launcher plus a base64 JSON record. The ID and options are embedded in
   that snapshot, so no independent sidecar mapping can drift from older saves.
   These records are encoded, not encrypted. Snapshots are written mode 0600;
   treat them as private, especially if launch options contain sensitive values.
4. Resurrect recreates the panes and executes the launcher. The launcher changes
   to the saved workspace, restores CODEX_HOME, and directly executes
   `codex resume ... <UUID>` using an argv array, not shell interpolation.

No exit-message parsing, directory-based guesses, `--last`, or fresh chats are
used. A shell left behind after exiting Codex is saved as a shell. An unknown or
ambiguous Codex process is omitted and reported, even with Resurrect's `:all:`
setting. Background/suspended jobs, remote app servers, and noninteractive
Codex commands are outside this integration's scope. `@resurrect-processes false`
continues to disable process restoration.

Explicit model/profile/config/sandbox/approval/add-dir settings and common
interactive flags are retained. Original prompts, image attachments, previous
resume selectors and worktree-creation requests are not replayed; the saved
workspace is used instead. Unknown flags fail closed and appear under `skipped`.
Arbitrary shell environment variables and changes made through in-session UI
settings are not captured by this bridge; Codex's own session persistence and
current config determine those settings on resume.

The fallback depends on Linux `/proc` access and Codex's current rollout metadata
format, which is not a stable public API. If Codex changes that format, a trusted
SessionStart hook can still supply the ID. If neither can identify it, the bridge
skips the pane rather than opening the wrong conversation.

## Save, restore and diagnose

```sh
# Show exact currently detectable mappings and any skipped panes.
~/.tmux/plugins/codex-resurrect/codex_tmux.py status

# Save now before a planned reboot (also captures current scrollback).
~/.tmux/plugins/tmux-resurrect/scripts/save.sh

# Inspect the last save report (IDs only, no transcript contents).
cat ~/.local/state/codex-tmux/last-save.json
```

With the configured prefix, manual save is `Ctrl-a`, then `Ctrl-s`; manual
restore is `Ctrl-a`, then `Ctrl-r`. Continuum saves every five minutes and
restores when a new tmux server starts. Starting tmux at boot is separate.
Use the correct TMUX socket environment when operating multiple tmux servers.

Only state captured at the last successful snapshot is restored. This reopens
conversation history; it does not recover process memory, child commands or
automatically submit a prompt to continue an interrupted turn. Keep the saved
Codex home/session data available on disk.

## Validation

```sh
python3 -m unittest discover -s tmux/codex-resurrect -p 'test_*.py' -v
```

Tests cover metadata filtering, ambiguous and stale mappings, option handling,
shell-safe execution and distinct conversations sharing a directory. The
integration test creates a temporary tmux server and compiled fake Codex binary,
saves through the real Resurrect script, shuts down the server and restores in
a new server. It checks exact IDs, cwd, profile, exit handling and slash-name
scrollback. It neither contacts Codex services nor touches live tmux sessions.

## Remove

Remove the Codex load line from tmux.conf and the matching SessionStart entry
from hooks.json; remove the two installed symlinks. Unset
`@resurrect-hook-post-save-layout` and remove `~codex-tmux-resume` from
`@resurrect-processes` in the running server. Snapshots made with the integration
need the launcher to resume Codex, so keep it until those snapshots are no longer
needed. Reverting the scrollback patch also makes encoded scrollback archives
incompatible; save a new snapshot afterward.
