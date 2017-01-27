#!/bin/bash
# @oxalorg

dest=$HOME/.local/bin

# symlink all files in child folders to local bin
for f in $(find . -mindepth 2 -type f -not -name README.md -not -path "*archived*")
do
    temp=`echo $f | cut --complement -c 1,2`
    src=`pwd`/$temp
    link=$dest/`basename $f`
    [ ! -e "$link" ] && ln -sv $src $link && chmod u+x $link
done

# clean bin directory
for f in $dest/*; do
    if [ ! -e "$f" ] ; then
        # bin file/symlink is invalid
        rm $f
        echo "Removing broken link: $f"
    fi
done
