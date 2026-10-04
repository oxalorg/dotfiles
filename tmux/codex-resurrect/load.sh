#!/usr/bin/env bash
set -euo pipefail
bridge="$HOME/.tmux/plugins/codex-resurrect/codex_tmux.py"
printf -v save_hook '%q save' "$bridge"
current=$(tmux show-option -gqv @resurrect-hook-post-save-layout)
if [[ -n "$current" && "$current" != "$save_hook" ]]; then
    tmux display-message 'Codex restore: existing post-save-layout hook needs manual integration'
    exit 1
fi
tmux set-option -g @resurrect-hook-post-save-layout "$save_hook"
processes=$(tmux show-option -gqv @resurrect-processes)
if [[ "$processes" != ':all:' && "$processes" != 'false' && "$processes" != *'~codex-tmux-resume'* ]]; then
    tmux set-option -g @resurrect-processes "${processes:+$processes }~codex-tmux-resume"
fi
