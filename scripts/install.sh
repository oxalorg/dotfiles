#!/bin/bash

dest=$HOME/.local/bin

for f in */*
do
	src=`pwd`/$f
	echo $src
	cp $src $dest/`basename $f`	
	chmod u+x $dest/`basename $f`
done
