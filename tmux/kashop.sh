#!/bin/sh

# Set Session Name
SESSION="shopbanao"
SESSIONEXISTS=$(tmux list-sessions | grep $SESSION)

# Only create tmux session if it doesn't already exist
if [ "$SESSIONEXISTS" = "" ]
then
    # Start New Session with our name
    tmux new-session -d -s $SESSION

    # Name first Pane and start zsh
    tmux rename-window -t 1 'kashop-server'
    tmux send-keys -t 'kashop-server' 'cd ~/projects/kashop' C-m 'workon kashop' C-m 'make run' C-m # Switch to bind script?

    tmux split-window -h -c ~/projects/kashop
    tmux send-keys -t 'kashop-server.right' 'yarn run start' C-m

    tmux new-window -t $SESSION:2 -n 'kashop-code'
    tmux send-keys -t 'kashop-code' 'cd ~/projects/kashop' C-m 'workon kashop' C-m

    # setup Writing window
    tmux new-window -t $SESSION:3 -n 'shopbanao-server'
    tmux send-keys -t 'shopbanao-server' 'cd ~/projects/shopbanao' C-m 'workon shopbanao' C-m 'make run' C-m # Switch to bind script?

    tmux split-window -h -c ~/projects/shopbanao
    tmux send-keys -t 'shopbanao-server.right' 'yarn run start' C-m

    tmux new-window -t $SESSION:4 -n 'shopbanao-code'
    tmux send-keys -t 'shopbanao-code' 'cd ~/projects/shopbanao' C-m 'workon shopbanao' C-m

    tmux new-window -t $SESSION:5 -n 'deploy'
    tmux send-keys -t 'deploy' 'cd ~/projects/ansibledj' C-m 'workon ansibledj' C-m
fi

tmux attach-session -t $SESSION:1
