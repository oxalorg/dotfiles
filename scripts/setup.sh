#!/bin/bash
# @oxalorg

dest=$HOME/.local/bin


# clean bin directory
for f in $dest/*; do
    if [ ! -e "$f" ] ; then
        # bin file/symlink is invalid
        rm $f
        echo "Removing broken link: $f"
    fi
done

# symlink all files in child folders to local bin
for f in $(find . -mindepth 2 -type f -not -name README.md -not -path "*archived*")
do
    if [ "$(uname -s)" == "Darwin" ]; then
        temp=`echo $f | awk '{print substr($0, 3)}'`
    else
        temp=`echo $f | cut --complement -c 1,2`
    fi
    src=`pwd`/$temp
    link=$dest/`basename $f`
    [ ! -e "$link" ] && ln -sv $src $link && chmod u+x $link
done

if [[ "$(uname)" == "Darwin" ]]; then
    echo "Unlinkin pbcopy and pbpaste as already on macos"
    rm $dest/pbcopy $dest/pbpaste
fi
