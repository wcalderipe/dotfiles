#!/usr/bin/env bash

set -exu

if [[ ! -d ~/.my-conf ]]; then
  git clone --bare git@github.com:wcalderipe/dotfiles.git $HOME/.my-conf
  git --git-dir=$HOME/.my-conf --work-tree=$HOME checkout
  echo -e '[status]\n  showUntrackedFiles = no' >> $HOME/.my-conf/config
fi

echo "Installing zprezto..."
git clone --recursive https://github.com/sorin-ionescu/prezto.git $HOME/.zprezto

echo "Configuring nvim..."
$HOME/.vimfiles/macos-setup.sh

$HOME/.shell/macos-preferences.sh
