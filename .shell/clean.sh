#! /bin/bash

echo "~> Removing dotfiles..."
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
  ~/.iterm \
  ~/.vim \
  ~/.emacs.d \
  ~/.lein \
  ~/.vscode/extensions.txt \
  ~/.vscode/settings.json

echo "~> DONE!"
