# The following lines were added by compinstall
PROFILE_STARTUP=false
if [[ "$PROFILE_STARTUP" == true ]]; then
    zmodload zsh/zprof # Output load-time statistics
    # http://zsh.sourceforge.net/Doc/Release/Prompt-Expansion.html
    PS4=$'%D{%M%S%.} %N:%i> '
    exec 3>&2 2>"/tmp/zshstat.$$"
    setopt xtrace prompt_subst
fi


# LS_COLORS='rs=0:di=01;34:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:su=37;41:sg=30;43:ca=30;41:tw=30;42:ow=34;42:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.dz=01;31:*.gz=01;31:*.lz=01;31:*.xz=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.war=01;31:*.ear=01;31:*.sar=01;31:*.rar=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.svg=01;35:*.svgz=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.webm=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.flv=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.cgm=01;35:*.emf=01;35:*.axv=01;35:*.anx=01;35:*.ogv=01;35:*.ogx=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.axa=00;36:*.oga=00;36:*.spx=00;36:*.xspf=00;36:';
# export LS_COLORS

# zstyle ':completion:*' completer _expand _complete _ignored _approximate
# zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
# zstyle ':completion:*' matcher-list 'm:{[:lower:]}={[:upper:]} m:{[:lower:][:upper:]}={[:upper:][:lower:]}' 
# zstyle ':completion:*' menu select=1
# zstyle ':completion:*' select-prompt %SScrolling active %l: current selection at %p%s
# zstyle :compinstall filename '/home/mitesh/.zshrc'

# autoload -Uz compinit
# compinit
# End of lines added by compinstall

##################################
#### Global variables and loaders.
##################################

# first things first
# set the default binding to vi style dammit
bindkey -v
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

PYTHONPATH=$PYTHONPATH:/usr/local/Cellar/python/2.7.12_2/bin/python
export PATH="/Users/ox/anaconda3/bin:$PATH:/Users/ox/Library/Python/3.5/bin:/Users/ox/.mybin"

# virtualenvwrapper
export WORKON_HOME=~/.venv
export VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python3
export VIRTUALENVWRAPPER_SCRIPT=/usr/local/bin/virtualenvwrapper.sh
source ~/Library/Python/3.5/bin/virtualenvwrapper_lazy.sh

###########################
#### Load custom aliases.
###########################

#for file in ${HOME}/.config/zsh/aliases/*; do
#   	source "$file";
#done

# Needs to be DONE
alias v=$EDITOR

# List commands
alias ls="ls -GpF"
alias ll="ls -lGpF"
alias lt="ls -ltGpF"
alias la="ls -aGpF"

# Git
alias gst='git status -sb'
alias ga='git add'
alias gcm='git commit -m'
## Git log
alias gl='git log --graph --color --decorate --all --stat -p'
## Git log summary
alias gls="git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"

# Colorize commands
alias grep='grep --color'
alias nyancat='pygmentize -g -O style=colorful'

# Custom directories
alias doc='cd /doc'
alias nt='cd /notes'
alias proj='cd /projects'

# quickedits
alias nvrc='nvim $HOME/.config/nvim/init.vim'
alias zshrc='nvim $HOME/.config/zsh/.zshrc'

# Note search
alias ns='nvim --cmd "cd /notes" "$(fzf)"'
# Note create
alias nc='cd /notes/$(find -L /notes -type d ! -path \*.git\* | sed "s|^/notes||" | fzf) && nvim '
# Edit scratch pad note
alias npad='nvim --cmd "cd /notes" /notes/scratch-pad.md'
alias cpad='cat /notes/scratch-pad.md'

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
	zgen load andrewferrier/fzf-z
	zgen load supercrabtree/k
	zgen load zsh-users/zsh-syntax-highlighting
	zgen load zsh-users/zsh-autosuggestions
	zgen save
fi

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
