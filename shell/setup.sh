#! /bin/bash

# Ask for the administrator password upfront
sudo -v

if [[ ! -d "$HOME/dev/dotfiles" ]]; then
  mkdir -p $HOME/dev
  git clone https://github.com/wcalderipe/dotfiles.git $HOME/dev/dotfiles
else
  echo "[INFO] dotfiles repository already cloned."
fi

dotfiles_path=$HOME/dev/dotfiles

ln -sf $dotfiles_path/vim         $HOME/.vim
ln -sf $dotfiles_path/emacs.d     $HOME/.emacs.d
ln -sf $dotfiles_path/lein        $HOME/.lein
ln -sf $dotfiles_path/shell       $HOME/.shell
ln -sf $dotfiles_path/vscode      $HOME/.vscode

ln -sf $dotfiles_path/.vimrc      $HOME
ln -sf $dotfiles_path/.zshrc      $HOME
ln -sf $dotfiles_path/.zpreztorc  $HOME
ln -sf $dotfiles_path/.gitconfig  $HOME
ln -sf $dotfiles_path/.gitignore  $HOME
ln -sf $dotfiles_path/.tmux.conf  $HOME

source $dotfiles_path/shell/functions

install-everything

my-gnome-setup

setup-gnome-terminal-base16-solarized-profile

