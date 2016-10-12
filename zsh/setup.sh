#!/bin/sh
DOTFILESDIR=~/Dropbox/Projects/dotfiles

echo "# Checking if zsh is installed"

if command -v zsh >/dev/null
then
	echo "ZSH is installed. Proceeding.."
else
	echo "ZSH is not installed. Exiting.."
	exit
fi

#############################
# Check for existing configs
#############################

if [ -e ~/.zshrc -o -L ~/.zshrc ]; then
        echo "Backing up existing .zshrc."
        mv ~/.zshrc ~/.zshrc.$(date +%F-%R).bak
fi

if [ -d ~/.config/zsh -o -L ~/.config/zsh ]; then
        echo "Backing up zsh folder"
        mv ~/.config/zsh ~/.config/zsh.$(date +%F-%R).bak
fi

################################
# Symlink .config/zsh and .zshrc
################################

echo "Soft linking zsh to ~/.config/zsh"
ln -s -v ${DOTFILESDIR}/zsh ${HOME}/.config/

echo "Soft linking .zshrc to ~/.zshrc"
ln -s -v ${HOME}/.config/zsh/.zshrc ${HOME}/

###############
# Install zgen
###############

if ! [ -e ~/.zgen -o -L ~/.zgen ]; then
	cd ~
	git clone https://github.com/tarjoilija/zgen .zgen
fi

echo "Now run 'source ~/.zshrc' to activate your settings."
