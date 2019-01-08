#! /bin/bash

# Ask for the administrator password upfront
sudo -v

if [[ ! -d "$HOME/dev/dotfiles" ]]; then
  mkdir -p $HOME/dev
  git clone https://github.com/wcalderipe/dotfiles.git $HOME/dev/dotfiles
  # TODO: Delete checkout step before merge linux branch into master
  cd $HOME/dev/dotfiles && git fetch && git checkout linux

  ln -sf $HOME/dev/dotfiles/.vim        $HOME
  ln -sf $HOME/dev/dotfiles/.vimrc      $HOME
  ln -sf $HOME/dev/dotfiles/.shell      $HOME
  ln -sf $HOME/dev/dotfiles/.vscode     $HOME
  ln -sf $HOME/dev/dotfiles/.zshrc      $HOME
  ln -sf $HOME/dev/dotfiles/.zpreztorc  $HOME
  ln -sf $HOME/dev/dotfiles/.gitconfig  $HOME
  ln -sf $HOME/dev/dotfiles/.gitignore  $HOME
  ln -sf $HOME/dev/dotfiles/.tmux.conf  $HOME
  ln -sf $HOME/dev/dotfiles/.iterm      $HOME
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
  install_ripgrep
  setup_gnome
fi
