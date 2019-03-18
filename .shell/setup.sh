#! /bin/bash

# Ask for the administrator password upfront
sudo -v

source $HOME/.shell/functions

clone-dotfiles

create-symlinks-to-dotfiles

install-everything

my-gnome-setup

setup-gnome-terminal-base16-solarized-profile

