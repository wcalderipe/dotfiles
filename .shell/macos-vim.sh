#!/usr/bin/env bash

pip3 install neovim

ln -s $HOME/.vim $HOME/.config/nvim

# Add vim-plug to autoload
if [[ ! -f "$HOME/.vim/autoload/plug.vim" ]]; then
  curl -fLo $HOME/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
fi
