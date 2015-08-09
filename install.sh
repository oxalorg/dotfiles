#!/bin/sh

function exit_if_failed{
	echo "# ERROR # Exiting.."
	echo 1>&2 "failed with $?"
	exit 1
} 

echo "\nInstalling essentional sw-packages"
cat sw-packages | xargs sudo apt-get --assume-yes install || exit_if_failed

echo "\nSoft linking ./xfce4 to ~/.config/"
ln --symbolic ~/dotfiles/xfce4/ ~/.config/xfce4 || exit_if_failed

echo "\nSoft linking ./.vim/ to ~/.vim/"
ln --symbolic ~/dotfiles/.vim/ ~/.vim || exit_if_failed

echo "\nDownloading fasd"
git clone https://github.com/clvv/fasd  || exit_if_failed
echo "\nFasd downloaded. Installing.." || exit_if_failed
make install

echo "\nSoft linking global .gitconfig.."
ln --symbolic ~/dotfiles/.gitconfig ~/.gitconfig
