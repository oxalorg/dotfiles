#!/bin/bash

DOTDIR=`pwd`

function exit_if_failed{
	echo "# ERROR # Exiting.."
	echo 1>&2 "failed with $?"
	exit 1
} 

#echo "\nInstalling essentional sw-packages"
#cat sw-packages | xargs sudo apt-get --assume-yes install || exit_if_failed

echo "\nSoft linking global gitconfig and gitignore.."
ln --symbolic $DOTDIR/gitconfig ~/.gitconfig
ln --symbolic $DOTDIR/gitignore-global ~/.gitignore-global


