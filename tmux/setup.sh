#!/bin/sh

if [ -d ~/.tmux/plugins/tpm ]; then
	echo "Tmux plugin manager already exists"
else
	git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
fi

if [ -f ~/.tmux.conf ]; then
        echo "Creating .tmux.conf backup."
        mv ~/.tmux.conf ~/.tmux.conf$(date +%F-%R).bak
fi

echo "Soft linking .tmux.conf to ${HOME} directory."
ln -s -v ${DOTFILESDIR}/tmux/.tmux.conf ${HOME}/
