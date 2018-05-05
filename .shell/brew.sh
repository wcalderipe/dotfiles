#!/bin/bash

set -x

install_homebrew () {
  which -s brew
  if [[ $? != 0 ]] ; then
    # Install homebrew
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    brew tap caskroom/cask
  fi
}

install_cask () {
  local installed=$1
  local package=$2

  grep --quiet "$package" <<< $installed
  if [[ $? != 0 ]]; then
    brew cask install $package
  fi
}

install_formula () {
  local installed=$1
  local formula=$2

  grep --quiet `awk '{print $1}' <<< $formula` <<< $installed
  if [[ $? != 0 ]]; then
    brew install $formula
  fi
}

switch_to_brew_installed_zsh () {
  if ! fgrep -q '/usr/local/bin/zsh' /etc/shells; then
    echo '/usr/local/bin/zsh' | sudo tee -a /etc/shells;
    chsh -s /usr/local/bin/zsh;
  fi
}

install_homebrew
brew tap caskroom/versions # For Java 7
brew update
brew upgrade

installed_formulas=`brew list`
declare -a formulas=( \
  "cmake" \
  "gnu-sed --with-default-names" \
  "git" \
  "tig" \
  "tmux" \
  "zsh" \
)

for formula in "${formulas[@]}"; do
  install_formula "$installed_formulas" "$formula"
done

installed_casks=`brew cask list`
declare -a casks=( \
  "atom" \
  "firefox" \
  "google-chrome" \
  "gpgtools" \
  "iterm2" \
  "skype" \
  "spotify" \
  "telegram" \
  "caffeine" \
)

for cask in "${casks[@]}"; do
  install_cask "$installed_casks", "$cask"
done

switch_to_brew_installed_zsh
brew cleanup
