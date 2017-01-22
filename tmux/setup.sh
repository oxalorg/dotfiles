#!/bin/sh
DOTFILESDIR=~/Dropbox/Projects/dotfiles
if [ -f ~/.tmux.conf ]; then
        echo "Creating .tmux.conf backup."
        mv ~/.tmux.conf ~/.tmux.conf$(date +%F-%R).bak
fi

echo "Soft linking .tmux.conf to ${HOME} directory."
ln -s -v ${DOTFILESDIR}/tmux/.tmux.conf ${HOME}/
