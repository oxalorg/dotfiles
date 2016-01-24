#!/bin/sh
DOTFILESDIR=~/.dotfiles

echo "# Checking if zsh is installed"

if command -v zsh >/dev/null
then
	echo "ZSH is installed. Proceeding.."
else
	echo "ZSH is not installed. Exiting.."
	exit
fi

echo "Soft linking zsh to ~/.config/zsh"
ln --symbolic ${DOTFILESDIR}/zsh ${HOME}/.config/

echo "Soft linking .zshrc to ~/.zshrc"
ln --symbolic ${HOME}/.config/zsh/.zshrc ${HOME}/

