#!/bin/sh

echo "Soft linking tmux.conf"
ln --symbolic $dotfiles/lone_configs/tmux.conf $HOME/.tmux.conf  

