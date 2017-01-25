# The following lines were added by compinstall
PROFILE_STARTUP=false
if [[ "$PROFILE_STARTUP" == true ]]; then
    zmodload zsh/zprof # Output load-time statistics
    # http://zsh.sourceforge.net/Doc/Release/Prompt-Expansion.html
    PS4=$'%D{%M%S%.} %N:%i> '
    exec 3>&2 2>"/tmp/zshstat.$$"
    setopt xtrace prompt_subst
fi

zstyle ':completion:*' completer _expand _complete _ignored _approximate
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' matcher-list 'm:{[:lower:]}={[:upper:]} m:{[:lower:][:upper:]}={[:upper:][:lower:]}' 
zstyle ':completion:*' menu select=1
zstyle ':completion:*' select-prompt %SScrolling active %l: current selection at %p%s
zstyle :compinstall filename '$HOME/.config/zsh/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

##################################
#### Global variables and loaders.
##################################

# first things first
# set the default binding to vi style dammit
bindkey -v
bindkey -v '^?' backward-delete-char
export EDITOR=nvim
autoload -Uz colors && colors
setopt autocd extendedglob

# report command time if > 'x' seconds
REPORTTIME=2

# moo gangsta
#moo

# neovim cursor support
export NVIM_TUI_ENABLE_CURSOR_SHAPE=1

######################
#### History Settings.
######################

if [ -z "$HISTFILE" ]; then
	HISTFILE=$HOME/.zsh_histfile
fi
HISTSIZE=100000
SAVEHIST=100000

# History stamps as: yyyy-mm-dd
alias history='fc -il 1'

setopt append_history
setopt hist_ignore_space
setopt hist_verify
setopt inc_append_history
setopt share_history # share command history data

######################
#### Load custom paths
######################

# source ${HOME}/.config/zsh/paths

# PYTHONPATH=$PYTHONPATH:/usr/local/Cellar/python/2.7.12_2/bin/python
# export PATH="/Users/ox/Library/Python/3.5/bin:/Users/ox/anaconda3/bin:/Users/ox/.mybin:$PATH"
export PATH="/Users/ox/Library/Python/3.5/bin:/Users/ox/.mybin:~/.local/bin:$PATH"

# virtualenvwrapper
export WORKON_HOME=~/.venv
if [[ -f ~/Library/Python/3.5/bin/virtualenvwrapper.sh ]]; then
    export VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python3
    export VIRTUALENVWRAPPER_SCRIPT=~/Library/Python/3.5/bin/virtualenvwrapper.sh
    source ~/Library/Python/3.5/bin/virtualenvwrapper_lazy.sh
else
    export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python3
    export VIRTUALENVWRAPPER_SCRIPT=~/.local/bin/virtualenvwrapper.sh
    source ~/.local/bin/virtualenvwrapper_lazy.sh
fi

###########################
#### Load custom aliases.
###########################

#for file in ${HOME}/.config/zsh/aliases/*; do
#   	source "$file";
#done
###########################
#### Load custom functions.
###########################

for file in ${HOME}/.config/zsh/functions/*; do
   	source "$file";
done

###########################
#### Load other scripts.
###########################
[ -f /usr/local/bin/z.sh ] && . /usr/local/bin/z.sh

### fzf - fuzzy finder
### https://github.com/junegunn/fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
# Setting ag as the default source for fzf
export FZF_DEFAULT_COMMAND='ag -g ""'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_COMPLETION_TRIGGER='~~'


##############################
#### Load 3rd party extentions.
###############################

### zgen
## https://github.com/tarjoilija/zgen
source "${HOME}/.zgen/zgen.zsh"

if ! zgen saved; then
	echo "Creating a zgen save"
    zgen oh-my-zsh plugins/vi-mode
	zgen load andrewferrier/fzf-z
	zgen load supercrabtree/k
	zgen load zsh-users/zsh-syntax-highlighting
	zgen load zsh-users/zsh-autosuggestions
    zgen load Vifon/deer
	zgen save
fi

## Plugin settings
autoload -U deer
zle -N deer
bindkey '\ek' deer
zstyle ':deer:' height 35

# My custom ninja theme
source "${HOME}/.config/zsh/ninja.zsh-theme"

# export locales
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8


# Make these utils more verbose
for c in cp rm chmod chown rename ln; do
    alias $c="$c -v"
done

# Force tmux to support 256 colors and UTF-8
alias tmux="tmux -2u"

# Custom command completions
fpath=($HOME/.config/zsh/custompletions $fpath)

if [[ "$PROFILE_STARTUP" == true ]]; then
    zprof
    unsetopt xtrace
    exec 2>&3 3>&-
fi
