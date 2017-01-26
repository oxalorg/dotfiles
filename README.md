# Welcome to oxalorgs home base - ark.

Less is more.
Beautiful is better than ugly.
Explicit is better than implicit.
Simple is better than complex.

---

## Installation

You can either run the below commands which will run `setup.sh`
from every folder, or you can manually run setup of the folders
you want to install.

```
#!/bin/bash

DOTDIR=`pwd`

function exit_if_failed{
    echo "# ERROR # Exiting.."
    echo 1>&2 "failed with $?"
    exit 1
} 

for f in */setup.sh; do
    bash $f | exit_if_failed
done
```

## Configurations currently in-use:

* shell - shell related config
* vim - Started using vim again (jan 2017)
* nvim
* csgo
* logkeys
* git
* ~~zsh~~ - Using prezto
* ~~xfce4~~ I've switched back to Unity (dec 2016)
* ~~atom~~ - Don't use it anymore, too slow
* ~~tmux~~ - Don't use it anymore, too complex

## Notes

Please edit the following files if you are cloning/forking this repository:

* git/.gitconfig [user.name and user.email]

## Changelogs

* 2017-01-27 Started using vscode for larger projects
* 2017-01-26 Removed atom and zsh folders. Fixed shell folder.
* 2017-01-25 Switched from custom zsh config to
  [Prezto](https://github.com/sorin-ionescu/prezto)
* 2017-01-21 Started using vim (along with neovim)


