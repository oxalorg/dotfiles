# zsh configuration

Currently running [slimzsh](https://github.com/changs/slimzsh).

# setup.sh explained

1. Checks if ZSH is installed.
	+ Goto 2. if installed
	+ Exit if not.
2. Download (clone) slimzsh repsitory from github and place it in HOME directory.
3. Soft link .zshrc from this directory to ~/.zshrc.
	+ .zshrc sources slim.zsh

