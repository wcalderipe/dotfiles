#! /bin/bash

echo "[INFO] Removing dotfiles..."

rm -rf \
  ~/.gitconfig \
  ~/.gitignore \
  ~/.tmux.conf \
  ~/.zpreztorc \
  ~/.zshrc \
  ~/.tmux \
  ~/.shell \
  ~/.zprezto \
  ~/.emacs.d \
  ~/.lein \

echo "[INFO] Done."
