#! /bin/bash

echo "[INFO] Removing dotfiles..."

rm -rf \
  ~/.gitconfig \
  ~/.gitignore \
  ~/.tmux.conf \
  ~/.vimrc \
  ~/.zpreztorc \
  ~/.zshrc \
  ~/.tmux \
  ~/.shell \
  ~/.zprezto \
  ~/.vim \
  ~/.emacs.d \
  ~/.lein \

echo "[INFO] Done."
