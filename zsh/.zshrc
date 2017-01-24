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

# Needs to be DONE
alias v=$EDITOR

# MPC MPD Aliases
alias mpn='mpc next'
alias mpt='mpc toggle'
alias mpp='mpc prev'
alias mppl='mpc playlist'
alias mpa='mpc add'

# List commands
alias ls="ls -GpF"
alias ll="ls -lGpF"
alias lt="ls -ltGpF"
alias la="ls -aGpF"

# Git
alias gst='git status -sb'
alias ga='git add'
alias gu='git add -u'
alias gcm='git commit -m'
alias gp='git push'
alias gc='git checkout'
alias gb='git branch'
## Git log
alias gl='git log --graph --color --decorate --all --stat -p'
alias gls="git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"

# Stylize commands
alias grep='grep --color'
alias nyancat='pygmentize -g -O style=colorful'
alias cpsakura='cat /projects/sakura.css/normalize.css /projects/sakura.css/sakura-light.css | pbcopy'

# Utils
alias dude='du -hcd 1 | gsort -hr'
alias t='tree'

# Custom sed
alias sedmdimg="sed 's/\(.*\)/![\1]\(\1\)/g'"


# Custom directories
alias doc='cd /doc'
alias proj='cd /projects'
ccd() {
    cd $OLDPWD
}

# quickedits
alias nvrc='nvim $HOME/.config/nvim/init.vim'
alias zshrc='nvim $HOME/.config/zsh/.zshrc'

# Super useful shortcuts
alias agg='ag -g'
alias agga='ag -g ""'
alias agd='find . -type d | ag'
alias -g AG='| ag'
alias -g G='| grep'
alias ftd='find . -type d'

# temp alias
alias ddystic='/projects/dystic/venv/bin/dystic'

########################
# Personal note taking #
########################

anti() {
    cd ~/Dropbox/cabinet/journal/antisocial
    local last=$(ls *.txt | sort -r | head -n 1 | cut -b 1-5)
    echo $last
    echo
    local post=$((last+1))
    local maxId=$(printf "%05d" $post)
    echo $(date -u +"%Y-%m-%dT%H:%M:%SZ") >> ${maxId}.txt
    echo "\n" >> ${maxId}.txt
    vim ${maxId}.txt
}

alias nt='cd /notes'
# Note search
ns() {
    local query=$@
    cd /notes
    nvim "$(fzf -1 -q $query)"
}
# Note create
nc() {
    local p=$(find -L /notes -type d ! -path \*.git\* | sed "s|^/notes||" | fzf -1 -q "$@")
    cd /notes/$p
    nvim
}
# Create snippet
# TODO: Add linux (xclip) support
nsnip() {
    local content=pbpaste
    local genname=`pbpaste | head -c 25`
    local genname=`xclip -o -selection clip | head -c 25`
    local filename=${@:-$genname}
    local slug=`echo $filename | sed 's|\ |_|g' | sed 's|[_]*$||'`
    $content >> /notes/snips/$(date +%F)-${slug}.md
    echo "\n" >> /notes/snips/$(date +%F)-${slug}.md
}

# Edit scratch pad note
alias npad='nvim --cmd "cd /notes" /notes/scratch-pad.md'
alias cpad='cat /notes/scratch-pad.md'
alias ppad='/notes/scratch-pad.md'

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
