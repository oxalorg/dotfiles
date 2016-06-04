#!/usr/bin/env sh
# source ../dotfiles_env.sh
echo "==========ATOM SETUP=============="

DOTFILES=$HOME/Dropbox/Projects/dotfiles
ATOM_DIR=$HOME/.atom

for f in *
do
    if [ "$f" = "setup.sh" ]
    then
        continue
    else
        ln -fvs $DOTFILES/atom/$f $ATOM_DIR/$f
    fi
done

echo "Now run: apm install < packages.list"

echo "================================="
