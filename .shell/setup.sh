#! /bin/bash

# Ask for the administrator password upfront
sudo -v

if [[ ! -d "$HOME/dev/dotfiles" ]]; then
  mkdir -p $HOME/dev
  local dotfiles_path=$HOME/dev/dotfiles
  git clone git@github.com:ilmotta/dotfiles.git $dotfiles_path
  ln -sf $dotfiles_path/.vim        $HOME
  ln -sf $dotfiles_path/.vimrc      $HOME
  ln -sf $dotfiles_path/.shell      $HOME
  ln -sf $dotfiles_path/.vscode     $HOME
  ln -sf $dotfiles_path/.zshrc      $HOME
  ln -sf $dotfiles_path/.zpreztorc  $HOME
  ln -sf $dotfiles_path/.gitconfig  $HOME
  ln -sf $dotfiles_path/.gitignore  $HOME
  ln -sf $dotfiles_path/.tmux.conf  $HOME
  ln -sf $dotfiles_path/.iterm      $HOME
else
  echo "~> dotfiles repository already cloned."
fi

source $HOME/.shell/function-shared
install_tmux_plugin_manager
install_node_version_manager
install_vim_plugin_manager
install_zprezto

if [[ "$(uname)" == "Darwin" ]]; then
  echo "MacOS detected..."
  source $HOME/.shell/function-macos
  setup_general_utilities
  setup_keyboard
  setup_finder
  setup_dock
  setup_appstore
fi

if [[ "$(expr substr $(uname -s) 1 5)" == "Linux" ]]; then
  echo "Linux detected..."
  source $HOME/.shell/function-ubuntu
  install_basic_dependencies
  install_docker
  install_npm
  install_google_chrome
  setup_gnome
fi
