#!/bin/bash

dest=$HOME/.mybin

for f in */*
do
	src=`pwd`/$f
	echo $src
	cp $src $dest/`basename $f`	
	chmod u+x $dest/`basename $f`
done
