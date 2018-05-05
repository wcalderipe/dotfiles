#!/usr/bin/env bash

set -exu

if [[ ! -d ~/.my-conf ]]; then
  git clone --bare git@github.com:wcalderipe/dotfiles.git $HOME/.my-conf
  git --git-dir=$HOME/.my-conf --work-tree=$HOME checkout
  echo -e '[status]\n  showUntrackedFiles = no' >> $HOME/.my-conf/config
fi

$HOME/.shell/macos-preferences.sh
