#!/usr/bin/env sh

# Required variables for third-party softwares (e.g. rbenv, jenv, etc...) are
# dynamically added to .zshenv via Ansible roles.

[[ -f ~/.zprezto/init.zsh ]] && source ~/.zprezto/init.zsh

[[ -f ~/.fzf.zsh ]] && source ~/.fzf.zsh

# Enable Ctrl-R for navigating through the history.
bindkey "^R" history-incremental-search-backward

[[ -f ~/vars ]] && source ~/vars
[[ -f ~/aliases ]] && source ~/aliases
[[ -f ~/private-variables ]] && source ~/private-variables

if [ "$(uname -s)" != "Darwin" ]; then
  # Base16 shell
  BASE16_SHELL="$HOME/.config/base16-shell/"

  [ -n "$PS1" ] && \
    [ -s "$BASE16_SHELL/profile_helper.sh" ] && \
    eval "$("$BASE16_SHELL/profile_helper.sh")"

  [[ -f "$BASE16_SHELL/profile_helper.sh" ]] && base16_zenburn
fi

# TODO: jEnv initialization in the .zshenv which isn't working on MacOS.
if [[ -d "$HOME/.jenv" ]]; then
  export PATH="$HOME/.jenv/bin:$PATH"
  eval "$(jenv init -)"
fi
