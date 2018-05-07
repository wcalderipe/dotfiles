#!/usr/bin/env bash

set -exu

if [[ ! -d ~/.my-conf ]]; then
  git clone --bare git@github.com:wcalderipe/dotfiles.git $HOME/.my-conf
  git --git-dir=$HOME/.my-conf --work-tree=$HOME checkout
  echo -e '[status]\n  showUntrackedFiles = no' >> $HOME/.my-conf/config
fi

echo "Installing zprezto..."
git clone --recursive https://github.com/sorin-ionescu/prezto.git $HOME/.zprezto

# Add vim-plug to autoload
if [[ ! -f "$HOME/.vim/autoload/plug.vim" ]]; then
  curl -fLo $HOME/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
fi

$HOME/.shell/macos-preferences.sh
