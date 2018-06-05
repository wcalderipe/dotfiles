#!/usr/bin/env bash

set -exu

if [[ ! -d ~/.my-conf ]]; then
  git clone --bare git@github.com:wcalderipe/dotfiles.git $HOME/.my-conf
  git --git-dir=$HOME/.my-conf --work-tree=$HOME checkout
  echo -e '[status]\n  showUntrackedFiles = no' >> $HOME/.my-conf/config
fi

echo "Installing zprezto..."
git clone --recursive https://github.com/sorin-ionescu/prezto.git $HOME/.zprezto

if [[ ! -f "$HOME/.vim/autoload/plug.vim" ]]; then
  echo "Installing vim-plug"
  curl -fLo $HOME/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
fi

if [[ ! -d ~/.nvm ]]; then
  echo "Installing Node Version Manager (NVM)..."
  git clone https://github.com/creationix/nvm.git $HOME/.nvm
fi

$HOME/.shell/macos-preferences.sh
