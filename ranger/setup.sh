#!/bin/bash
DOTFILES=/dotfiles

if [ -z "$DOTFILES" ]; then
    echo "DOTFILES var is not set."
    exit 1
fi

ln -s $DOTFILES/ranger $HOME/.config/ranger
