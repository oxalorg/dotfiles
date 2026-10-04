#!/usr/bin/env python3
"""Install local bridge and merge its Codex hook without replacing user hooks."""
import json
from pathlib import Path
import shlex
import shutil

from codex_tmux import atomic_write

HERE = Path(__file__).resolve().parent
HOME = Path.home()


def link(source, target):
    target.parent.mkdir(parents=True, exist_ok=True)
    if target.is_symlink() and target.resolve() == source.resolve():
        return
    if target.exists() or target.is_symlink():
        raise RuntimeError(f"refusing to replace {target}")
    target.symlink_to(source)


def main():
    link(HERE, HOME / ".tmux/plugins/codex-resurrect")
    link(HERE / "codex_tmux.py", HOME / ".local/bin/codex-tmux-resume")
    hooks_path = HOME / ".codex/hooks.json"
    data = json.loads(hooks_path.read_text()) if hooks_path.exists() else {}
    command = shlex.join([str(HOME / ".tmux/plugins/codex-resurrect/codex_tmux.py"), "hook"])
    entries = data.setdefault("hooks", {}).setdefault("SessionStart", [])
    if not any(h.get("command") == command for entry in entries for h in entry.get("hooks", [])):
        if hooks_path.exists():
            shutil.copy2(hooks_path, hooks_path.with_suffix(".json.before-codex-tmux"))
        entries.append({"matcher": "startup|resume|clear|compact", "hooks": [
            {"type": "command", "command": command, "timeout": 5}
        ]})
        atomic_write(hooks_path, json.dumps(data, indent=2) + "\n")

    # Resurrect uses the same filename helper for saving and restoring scrollback.
    # Percent-escape before slash-escape to avoid collisions (a/b versus a%2Fb).
    helpers = HOME / ".tmux/plugins/tmux-resurrect/scripts/helpers.sh"
    old = '\tlocal pane_id="$2"\n\techo "$(pane_contents_dir "$save_or_restore")/pane-${pane_id}"'
    new = '\tlocal pane_id="$2"\n\t# codex-tmux: encode session-name path separators in both directions.\n\tpane_id="${pane_id//%/%25}"\n\tpane_id="${pane_id//\\//%2F}"\n\techo "$(pane_contents_dir "$save_or_restore")/pane-${pane_id}"'
    text = helpers.read_text()
    if new not in text:
        if text.count(old) != 1:
            raise RuntimeError("Resurrect helper changed; scrollback patch requires review")
        mode = helpers.stat().st_mode & 0o777
        shutil.copy2(helpers, helpers.with_suffix(".sh.before-codex-tmux"))
        atomic_write(helpers, text.replace(old, new))
        helpers.chmod(mode)
    print("Installed. Reload tmux.conf. Review the SessionStart hook with Codex /hooks.")


if __name__ == "__main__":
    main()
