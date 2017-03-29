# Shell config

I dislike having to customise my shells the same way I customise
vimrc or other programs. One of the major reasons for that is
becuase I use both zsh and bash.

zsh - use it on all personal/home pc
bash - use it on all servers and also write all personal scripts in bash

## Install

    ln -s $DOTFILES/shell/aliases ~/.aliases
    ln -s $DOTFILES/shell/bashrc ~/.oxbashrc
    ln -s $DOTFILES/shell/zshrc ~/.oxzshrc
    ln -s $DOTFILES/shell/zpreztorc ~/.oxzpreztorc

Now you can source these from the local files.

    [ -f ~/.oxbashrc ] && source ~/.oxbashrc
    [ -f ~/.oxzshrc ] && source ~/.oxzshrc

And add this line to `~/.zpreztorc` installed by the installer

    [ -f ~/.oxzpreztorc ] && source ~/.oxzpreztorc

## Notes

Since I dislike having customised shells, I offload the majority
of workload to independent scripts which I can use anywhere.

They're collected in a local bin directory. So make sure to
install those. == Check `../scripts` ==
