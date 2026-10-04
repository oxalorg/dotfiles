import base64
import io
import json
import os
from pathlib import Path
import shlex
import shutil
import subprocess
import tempfile
import time
import unittest
from unittest.mock import patch

import codex_tmux as bridge


SID = "01a0ea30-5738-7123-be7e-eac583455413"
OTHER = "01a0ea30-5738-7123-be7e-eac583455414"
HERE = Path(__file__).resolve().parent
RESURRECT = Path.home() / ".tmux/plugins/tmux-resurrect"


class BridgeTests(unittest.TestCase):
    def test_launch_settings_preserved_without_replaying_prompt(self):
        args = ["codex", "-c", 'model_reasoning_effort="high"', "resume", "--last",
                "--profile=work", "--cd", "/old", "--worktree", "--image", "a.png",
                "--no-alt-screen", "previous prompt"]
        self.assertEqual(bridge.resume_options(args),
                         ["-c", 'model_reasoning_effort="high"', "--profile", "work", "--no-alt-screen"])
        for args in [["codex", "--remote", "ws://localhost"], ["codex", "exec"],
                     ["codex", "--unknown"], ["codex", "app-server"]]:
            with self.assertRaises(ValueError):
                bridge.resume_options(args)

    def test_snapshot_maps_same_directory_to_distinct_ids(self):
        def row(window):
            return f"pane\tproject/name\t{window}\t1\t:*\t1\t:title\t:/tmp\t1\tcodex\t:codex\n"
        records = {("project/name", "1", "1"): {"id": SID},
                   ("project/name", "2", "1"): {"id": OTHER}}
        result = bridge.rewrite_snapshot(row(1) + row(2) + row(3), records, {}, "/bin/launcher")
        rows = result.splitlines()
        for row, sid in zip(rows, [SID, OTHER]):
            command = row.split("\t")[10][1:]
            payload = json.loads(base64.urlsafe_b64decode(shlex.split(command)[1]))
            self.assertEqual(payload["id"], sid)
        self.assertEqual(rows[2].split("\t")[10], ":")

    def test_root_metadata_excludes_subagents(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "rollout.jsonl"
            for source, expected in [("cli", SID), ({"subagent": {"other": "guardian"}}, None)]:
                path.write_text(json.dumps({"type": "session_meta", "payload": {
                    "id": SID, "cwd": tmp, "source": source}}) + "\n")
                result = bridge.root_metadata(path)
                self.assertEqual(result["id"] if result else None, expected)

    def test_ambiguity_and_stale_hook_fail_closed(self):
        p = {"pid": 123, "start": "45"}
        with tempfile.TemporaryDirectory() as tmp:
            cache = Path(tmp) / "cache"
            cache.write_text(json.dumps({"id": SID, "cwd": tmp}))
            with patch.object(bridge, "cache_path", return_value=cache), patch.object(
                    bridge, "open_sessions", return_value={OTHER: {"id": OTHER, "cwd": tmp}}):
                self.assertEqual(bridge.session_for(p)["id"], OTHER)
            cache.unlink()
            with patch.object(bridge, "cache_path", return_value=cache), patch.object(
                    bridge, "open_sessions", return_value={SID: {}, OTHER: {}}):
                with self.assertRaises(ValueError):
                    bridge.session_for(p)

    def test_hook_records_exact_id_and_ignores_other_events(self):
        with tempfile.TemporaryDirectory() as tmp:
            target = Path(tmp) / "binding"
            event = {"hook_event_name": "SessionStart", "session_id": SID, "cwd": tmp}
            with patch.dict(os.environ, {"TMUX_PANE": "%7"}), patch.object(
                    bridge, "proc", return_value={"argv": ["codex"]}), patch.object(
                    bridge, "cache_path", return_value=target), patch("sys.stdin", io.StringIO(json.dumps(event))):
                bridge.hook()
            self.assertEqual(json.loads(target.read_text())["id"], SID)

    def test_resume_uses_argv_not_shell(self):
        record = {"version": 1, "id": SID, "cwd": "/tmp/a ' b", "codex_home": "/tmp/home",
                  "options": ["-c", 'example="$(touch /tmp/should-not-exist)"']}
        with patch.object(os, "chdir") as chdir, patch.object(os, "execvp") as execute, patch.dict(os.environ):
            bridge.resume(bridge.encode(record))
            chdir.assert_called_once_with(record["cwd"])
            execute.assert_called_once_with("codex", ["codex", "resume", *record["options"], SID])


class RestoreIntegration(unittest.TestCase):
    """Real save/kill-server/restore with a fake Codex binary. Never contacts an API."""
    @unittest.skipUnless(RESURRECT.exists() and shutil.which("cc") and shutil.which("tmux"),
                         "requires installed Resurrect, tmux and cc")
    def test_reboot_layout_exact_sessions_and_exited_process(self):
        with tempfile.TemporaryDirectory(prefix="codex-tmux-test-") as tmp:
            root = Path(tmp)
            home = root / "home"
            home.mkdir()
            project = root / "project with spaces"
            project.mkdir()
            bindir = root / "bin"
            bindir.mkdir()
            code = root / "fake.c"
            code.write_text(r'''
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
int main(int argc, char **argv) {
  const char *id = getenv("TEST_SESSION");
  if (argc > 1 && !strcmp(argv[1], "resume")) {
    id = argv[argc-1];
    char log[4096]; snprintf(log, sizeof(log), "%s/%s.resumed", getenv("TEST_ROOT"), id);
    FILE *out = fopen(log, "w");
    if (!out) return 2;
    char cwd[4096]; getcwd(cwd, sizeof(cwd)); fprintf(out, "%s\n", cwd);
    for (int i = 1; i < argc; i++) fprintf(out, "%s\n", argv[i]);
    fclose(out);
  }
  char path[4096]; snprintf(path, sizeof(path), "%s/rollout-%s.jsonl", getenv("TEST_ROOT"), id);
  FILE *f = fopen(path, "r"); if (!f) return 3;
  puts("fake Codex running"); fflush(stdout);
  while (1) pause();
}
''')
            subprocess.run(["cc", str(code), "-o", str(bindir / "codex")], check=True)
            for sid in [SID, OTHER]:
                (root / f"rollout-{sid}.jsonl").write_text(json.dumps({"type": "session_meta", "payload": {
                    "id": sid, "source": "cli", "cwd": str(project)}}) + "\n")
            plugin = home / ".tmux/plugins/tmux-resurrect"
            shutil.copytree(RESURRECT, plugin)
            env = dict(os.environ, HOME=str(home), XDG_STATE_HOME=str(root / "state"),
                       XDG_DATA_HOME=str(root / "data"), PATH=f"{bindir}:{os.environ['PATH']}",
                       TEST_ROOT=tmp, CODEX_HOME=str(home / ".codex"))
            env.pop("TMUX", None)
            env.pop("TMUX_PANE", None)
            subprocess.run(["python3", str(HERE / "install.py")], env=env, check=True, capture_output=True)
            socket = str(root / "socket")
            def tmux(*args):
                return subprocess.check_output(["tmux", "-S", socket, *args], env=env, text=True).strip()
            def wait_for(predicate):
                deadline = time.monotonic() + 10
                while time.monotonic() < deadline:
                    if predicate():
                        return
                    time.sleep(.1)
                self.fail("timed out waiting for test process")
            def start():
                tmux("-f", "/dev/null", "new-session", "-d", "-s", "seed", "-c", str(project),
                     "/bin/bash --noprofile --norc")
                tmux("set-option", "-g", "default-shell", "/bin/bash")
                tmux("set-option", "-g", "default-command", "/bin/bash --noprofile --norc")
                tmux("set-option", "-g", "@resurrect-dir", str(root / "snapshots"))
                tmux("set-option", "-g", "@resurrect-capture-pane-contents", "on")
                tmux("set-option", "-g", "@resurrect-hook-post-save-layout",
                     shlex.join([str(HERE / "codex_tmux.py"), "save"]))
                tmux("set-option", "-g", "@resurrect-processes", "~codex-tmux-resume")
                tmux("set-option", "-g", "@resurrect-strategy-vim", "")
            def plugin_env():
                return dict(env, TMUX=f"{socket},{tmux('display-message', '-p', '#{pid}')},0")
            def stop():
                pids = [int(pid) for pid in tmux("list-panes", "-a", "-F", "#{pane_pid}").splitlines()]
                pids.append(int(tmux("display-message", "-p", "#{pid}")))
                tmux("kill-server")
                wait_for(lambda: all(not (p := bridge.proc(pid)) or p["state"] == "Z" for pid in pids))
            try:
                start()
                tmux("rename-session", "-t", "seed", "project/name")
                first = tmux("display-message", "-p", "-t", "project/name", "#{pane_id}")
                tmux("new-window", "-t", "project/name", "-c", str(project))
                second = tmux("display-message", "-p", "-t", "project/name", "#{pane_id}")
                tmux("send-keys", "-t", first, f"TEST_SESSION={SID} codex --profile work", "Enter")
                tmux("send-keys", "-t", second, f"TEST_SESSION={OTHER} codex", "Enter")
                def ready():
                    result = subprocess.check_output(["python3", str(HERE / "codex_tmux.py"), "status"],
                                                     env=plugin_env(), text=True)
                    return len(json.loads(result)["ready"]) == 2
                wait_for(ready)
                subprocess.run([str(plugin / "scripts/save.sh"), "quiet"], env=plugin_env(),
                               check=True, capture_output=True)
                snapshot = (root / "snapshots/last").resolve()
                saved = snapshot.read_text()
                self.assertEqual(saved.count("codex-tmux-resume"), 2)
                # Exiting a TUI must remove its binding from the next snapshot.
                tmux("send-keys", "-t", second, "C-c")
                wait_for(lambda: tmux("display-message", "-p", "-t", second,
                                      "#{pane_current_command}") == "bash")
                check = root / "after-exit.txt"
                check.write_text(saved)
                subprocess.run(["python3", str(HERE / "codex_tmux.py"), "save", str(check)],
                               env=plugin_env(), check=True, capture_output=True)
                self.assertEqual(check.read_text().count("codex-tmux-resume"), 1)
                stop()
                socket = str(root / "restored-socket")
                start()
                result = subprocess.run([str(plugin / "scripts/restore.sh")], env=plugin_env(),
                                        capture_output=True, text=True, timeout=30)
                self.assertEqual(result.returncode, 0, result.stderr)
                wait_for(lambda: all((root / f"{sid}.resumed").exists() for sid in [SID, OTHER]))
                for sid in [SID, OTHER]:
                    lines = (root / f"{sid}.resumed").read_text().splitlines()
                    self.assertEqual(lines[0], str(project))
                    self.assertEqual(lines[-1], sid)
                self.assertIn("--profile\nwork", (root / f"{SID}.resumed").read_text())
                # Slash-containing session scrollback was archived using a flat filename.
                import tarfile
                with tarfile.open(root / "snapshots/pane_contents.tar.gz") as archive:
                    self.assertTrue(any("project%2Fname" in name for name in archive.getnames()))
            finally:
                if subprocess.run(["tmux", "-S", socket, "has-session"], env=env, capture_output=True).returncode == 0:
                    stop()


if __name__ == "__main__":
    unittest.main()
