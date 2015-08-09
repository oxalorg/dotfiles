#!/bin/sh

echo "# Checking if zsh is installed"

if command -v zsh >/dev/null
then
	echo "ZSH is installed. Proceeding.."
else
	echo "ZSH is not installed. Exiting.."
	exit
fi

echo "Soft linking .zshrc to ~/.zshrc"
ln --symbolic ~/dotfiles/zsh/.zshrc ~/.zshrc

