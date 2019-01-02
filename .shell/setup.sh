#! /bin/bash

# Ask for the administrator password upfront
sudo -v

source $HOME/.shell/function-shared

clone_dotfiles
create_symlinks_to_dotfiles
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
