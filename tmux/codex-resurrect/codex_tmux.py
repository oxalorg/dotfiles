#!/usr/bin/env python3
"""Linux Codex/resurrect bridge. Uses only stdlib; never guesses a session by cwd."""
import argparse
import base64
import json
import os
from pathlib import Path
import shlex
import subprocess
import sys
import tempfile
import uuid


STATE = Path(os.environ.get("XDG_STATE_HOME", str(Path.home() / ".local/state"))) / "codex-tmux"
BOOT = Path("/proc/sys/kernel/random/boot_id").read_text().strip()
VALUE_OPTIONS = {
    "-c", "--config", "-m", "--model", "-p", "--profile", "-s", "--sandbox",
    "-a", "--ask-for-approval", "--enable", "--disable", "--add-dir",
    "--local-provider",
}
BOOL_OPTIONS = {
    "--oss", "--search", "--no-alt-screen", "--approve-for-me",
    "--dangerously-bypass-approvals-and-sandbox", "--strict-config",
}


def atomic_write(path, text):
    path.parent.mkdir(parents=True, exist_ok=True)
    fd, temp = tempfile.mkstemp(dir=path.parent, prefix=".codex-tmux-")
    try:
        with os.fdopen(fd, "w") as out:
            out.write(text)
            out.flush()
            os.fsync(out.fileno())
        os.replace(temp, path)
    finally:
        if os.path.exists(temp):
            os.unlink(temp)


def proc(pid):
    root = Path("/proc") / str(pid)
    try:
        fields = (root / "stat").read_text().rsplit(")", 1)[1].split()
        argv = [os.fsdecode(x) for x in (root / "cmdline").read_bytes().split(b"\0") if x]
        return dict(pid=int(pid), state=fields[0], ppid=int(fields[1]), pgrp=int(fields[2]),
                    tty=int(fields[4]), foreground=int(fields[5]), start=fields[19], argv=argv)
    except (OSError, ValueError, IndexError):
        return None


def is_codex(p):
    return p and p["argv"] and Path(p["argv"][0]).name == "codex"


def env_for(pid):
    return dict(item.split("=", 1) for item in
                Path(f"/proc/{pid}/environ").read_text().split("\0") if "=" in item)


def cache_path(p):
    return STATE / f'{BOOT}-{p["pid"]}-{p["start"]}.json'


def root_metadata(path):
    """Only read the metadata line, never prompts/tool output. Fail closed on format changes."""
    try:
        with Path(path).open() as f:
            row = json.loads(f.readline(1024 * 1024))
        meta = row["payload"]
        if row["type"] != "session_meta" or meta.get("source") != "cli":
            return None
        return {"id": str(uuid.UUID(meta["id"])), "cwd": meta["cwd"]}
    except (OSError, ValueError, KeyError, TypeError):
        return None


def open_sessions(pid):
    sessions = {}
    for fd in Path(f"/proc/{pid}/fd").iterdir():
        try:
            path = Path(os.readlink(fd))
            if path.name.startswith("rollout-") and path.suffix == ".jsonl":
                meta = root_metadata(path)
                if meta:
                    sessions[meta["id"]] = meta
        except OSError:
            continue
    return sessions


def session_for(p):
    sessions = open_sessions(p["pid"])
    try:
        cached = json.loads(cache_path(p).read_text())
        # A stale hook binding must not override a newly opened conversation.
        if not sessions or cached["id"] in sessions:
            return cached
    except (OSError, ValueError, KeyError):
        pass
    if len(sessions) == 1:
        return next(iter(sessions.values()))
    raise ValueError(f"cannot identify one active root conversation ({len(sessions)} candidates)")


def resume_options(argv):
    """Keep explicit settings, drop the old prompt/session selector and input images.

    Unknown flags and non-interactive/remote modes fail closed instead of silently
    changing behavior. Worktree creation is replaced by the recorded actual cwd.
    """
    result = []
    args = iter(argv[1:])
    positional = False
    for arg in args:
        if arg == "--":
            break
        key, equals, value = arg.partition("=")
        if key in VALUE_OPTIONS:
            result.extend([key, value if equals else next(args)])
        elif arg in BOOL_OPTIONS:
            result.append(arg)
        elif key in {"-C", "--cd", "-i", "--image"}:
            if not equals:
                next(args)
        elif arg in {"--last", "--all", "--include-non-interactive", "--worktree"}:
            continue
        elif arg.startswith("-"):
            raise ValueError(f"unsupported launch option {key}")
        elif not positional:
            if arg in {"exec", "e", "app-server", "exec-server", "mcp-server", "review"}:
                raise ValueError(f"not a local interactive session: {arg}")
            positional = True
        # Other positional args are previous prompts or resume/fork selectors.
    return result


def process_table():
    return {p["pid"]: p for path in Path("/proc").glob("[0-9]*") if (p := proc(path.name))}


def foreground_codex(root_pid, table):
    candidates = []
    for p in table.values():
        if not is_codex(p) or p["tty"] == 0 or p["pgrp"] != p["foreground"]:
            continue
        current, seen = p, set()
        while current and current["pid"] not in seen:
            seen.add(current["pid"])
            if current["pid"] == root_pid:
                candidates.append(p)
                break
            current = table.get(current["ppid"])
            # Do not mistake an agent-launched Codex child for the pane's TUI.
            if current and current["pid"] != root_pid and is_codex(current):
                break
    if len(candidates) > 1:
        raise ValueError("multiple foreground Codex processes")
    return candidates[0] if candidates else None


def record_for(p):
    meta = session_for(p)
    env = env_for(p["pid"])
    home = env.get("CODEX_HOME") or str(Path.home() / ".codex")
    if not Path(home).is_absolute():
        raise ValueError("relative CODEX_HOME cannot be restored reliably")
    return {"version": 1, "id": meta["id"], "cwd": meta["cwd"],
            "codex_home": home, "options": resume_options(p["argv"])}


def tmux(*args):
    return subprocess.check_output(["tmux", *args], text=True).rstrip("\n")


def panes():
    fmt = "\t".join("#{" + k + "}" for k in
                    ["session_name", "window_index", "pane_index", "pane_pid", "pane_id"])
    return [line.split("\t") for line in tmux("list-panes", "-a", "-F", fmt).splitlines()]


def collect():
    table = process_table()
    records, errors = {}, {}
    for session, window, pane, pid, pane_id in panes():
        key = (session, window, pane)
        try:
            p = foreground_codex(int(pid), table)
            if p:
                records[key] = record_for(p)
        except (OSError, ValueError, StopIteration) as exc:
            errors[key] = str(exc)
    return records, errors


def encode(record):
    return base64.urlsafe_b64encode(json.dumps(record, separators=(",", ":")).encode()).decode()


def rewrite_snapshot(text, records, errors, launcher):
    lines = []
    for line in text.splitlines():
        fields = line.split("\t")
        if fields[0] == "pane" and len(fields) == 11:
            key = (fields[1], fields[2], fields[5])
            if key in records:
                fields[10] = ":" + shlex.join([str(launcher), encode(records[key])])
            elif key in errors or fields[9] == "codex":
                # Even with @resurrect-processes ':all:', don't launch a fresh chat.
                fields[10] = ":"
            line = "\t".join(fields)
        lines.append(line)
    return "\n".join(lines) + "\n"


def save(snapshot):
    records, errors = collect()
    launcher = Path.home() / ".local/bin/codex-tmux-resume"
    atomic_write(snapshot, rewrite_snapshot(snapshot.read_text(), records, errors, launcher))
    atomic_write(STATE / "last-save.json", json.dumps({
        "snapshot": str(snapshot), "saved": {":".join(k): v["id"] for k, v in records.items()},
        "skipped": {":".join(k): v for k, v in errors.items()},
    }, indent=2) + "\n")
    for key, error in errors.items():
        print(f"codex-tmux: skipped {key}: {error}", file=sys.stderr)


def hook():
    data = json.load(sys.stdin)
    if not os.environ.get("TMUX_PANE"):
        return
    # Subagent hooks carry the parent's session ID: only track SessionStart.
    if data.get("hook_event_name") != "SessionStart":
        return
    p = proc(os.getppid())
    while p and not is_codex(p):
        p = proc(p["ppid"])
    if not p:
        return
    meta = {"id": str(uuid.UUID(data["session_id"])), "cwd": data["cwd"]}
    atomic_write(cache_path(p), json.dumps(meta))


def resume(token):
    record = json.loads(base64.b64decode(token, altchars=b"-_", validate=True))
    if record["version"] != 1:
        raise ValueError("unknown snapshot version")
    session_id = str(uuid.UUID(record["id"]))
    options = record["options"]
    if not isinstance(options, list) or not all(isinstance(x, str) for x in options):
        raise ValueError("invalid saved options")
    if resume_options(["codex", *options]) != options:
        raise ValueError("invalid saved launch options")
    os.chdir(record["cwd"])
    os.environ["CODEX_HOME"] = record["codex_home"]
    # exec argv directly: no shell expansion of user paths or configuration values.
    os.execvp("codex", ["codex", "resume", *options, session_id])


def main():
    if Path(sys.argv[0]).name == "codex-tmux-resume":
        resume(sys.argv[1])
        return
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="action", required=True)
    sub.add_parser("hook")
    sub.add_parser("status")
    saving = sub.add_parser("save")
    saving.add_argument("snapshot", type=Path)
    args = parser.parse_args()
    if args.action == "hook":
        hook()
    elif args.action == "save":
        save(args.snapshot)
    else:
        records, errors = collect()
        print(json.dumps({"ready": {":".join(k): v for k, v in records.items()},
                          "skipped": {":".join(k): v for k, v in errors.items()}}, indent=2))


if __name__ == "__main__":
    try:
        main()
    except (OSError, ValueError, KeyError, StopIteration, subprocess.CalledProcessError) as exc:
        print(f"codex-tmux: {exc}", file=sys.stderr)
        sys.exit(1)
