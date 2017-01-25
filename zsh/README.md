# zsh configuration

** CHECK ../shell **

I avoid using frameworks for zsh since most of them are bloated, i.e. contain a lot of excessive functions and options.

I have a custom zsh cofiguration picked and parceled from several sources, and I use [zgen](https://github.com/tarjoilija/zgen) plugin manager for minimal overhead. 

# setup.sh explained

1. Checks if ZSH is installed.
	+ Goto 2. if installed
	+ Exit if not.
2. Download (clone) slimzsh repsitory from github and place it in HOME directory.
3. Soft link the entire zsh directory from this repo to ~/.config/zsh
4. Soft link ~/.config/zsh/.zshrc to ~/.zshrc 
	+ .zshrc sources zgen in ~/.zgen/zgen.zsh

