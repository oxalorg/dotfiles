#!/bin/sh
DOTFILESDIR=~/Dropbox/Projects/dotfiles
echo ".vim setup initialized."


install_vim_plug () {
	curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
}


if [ -f ~/.vimrc ]; then
	echo "Backing up existing vimrc."
	mv ~/.vimrc ~/.vimrc.$(date +%F-%R).bak
fi

if [ -d ~/.vim ]; then
	echo "Backing up existing .vim folder"
	mv ~/.vim ~/.vim.$(date +%F-%R).bak
fi


echo "Linking .vim folder"
ln -s -v ${DOTFILESDIR}/.vim ~/


while true; do
    read -p "Do you wish to install vim-plug? [y/n]" yn
    case $yn in
        [Yy]* ) install_vim_plug; break;;
        [Nn]* ) exit;;
        * ) echo "Please answer [y]es or [n]o.";;
    esac
done
