#!/bin/sh

echo "# Checking if zsh is installed"

if command -v zsh >/dev/null
then
	echo "ZSH is installed. Proceeding.."
else
	echo "ZSH is not installed. Exiting.."
	exit
fi

git clone --recursive https://github.com/changs/slimzsh.git ~/.slimzsh || { echo "Failed" ; exit 1 }

echo "Soft linking .zshrc to ~/.zshrc"
ln --symbolic ~/dotfiles/zsh/.zshrc ~/.zshrc

