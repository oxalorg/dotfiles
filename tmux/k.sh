#!/bin/sh

# Set Session Name
SESSION="k"
SESSIONEXISTS=$(tmux list-sessions | grep $SESSION)

# Only create tmux session if it doesn't already exist
if [ "$SESSIONEXISTS" = "" ]
then
    # Start New Session with our name
    tmux new-session -d -s $SESSION

    # Name first Pane and start zsh
    tmux rename-window -t 1 'server'
    tmux send-keys -t 'server' 'cd ~/projects/kashop' C-m 'workon kashop' C-m # Switch to bind script?

    tmux split-window -h -c '~/projects/kashop'
fi

tmux attach-session -t $SESSION:1
