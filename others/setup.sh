#!/bin/bash

if [ -z "$DOTFILES" ]; then
    echo "DOTFILES var is not set."
    exit 1
fi

ln -s $DOTFILES/others/pylintrc $HOME/.pylintrc
