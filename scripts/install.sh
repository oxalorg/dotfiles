#!/bin/bash
# @oxalorg

dest=$HOME/.local/bin

# symlink all files in child folders to local bin
for f in $(find . -mindepth 2 -type f -not -name README.md)
do
    temp=`echo $f | cut --complement -c 1,2`
    src=`pwd`/$temp
    [ ! -e "$src" ] && ln -sv $src $dest/`basename $f` && chmod u+x $dest/`basename $f`
done

# clean bin directory
for f in $dest/*; do
    if [ ! -e "$f" ] ; then
        # bin file/symlink is invalid
        rm $f
    fi
done
