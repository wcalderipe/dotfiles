#!/usr/bin/env sh

[[ -f ~/.zprezto/init.zsh ]] && source ~/.zprezto/init.zsh

# Default system variables.
[[ -f ~/.shell/variables ]] && source ~/.shell/variables

# Dependencies required variables (e.g. rbenv, jenv, yarn).
# This file is created during the Ansible playbook and edited by
# tasks, therefore it shouldn't be versioned.
[[ -f ~/.composed-variables ]] && source ~/.composed-variables

[[ -f ~/private-variables ]] && source ~/private-variables

[[ -f ~/.shell/aliases ]] && source ~/.shell/aliases

[[ -f ~/.nvm/nvm.sh ]] && source ~/.nvm/nvm.sh

[[ -f ~/.fzf.zsh ]] && source ~/.fzf.zsh

# Enable Ctrl-R for navigating through the history.
bindkey "^R" history-incremental-search-backward

# Base16 shell
BASE16_SHELL="$HOME/.config/base16-shell/"

[ -n "$PS1" ] && \
    [ -s "$BASE16_SHELL/profile_helper.sh" ] && \
        eval "$("$BASE16_SHELL/profile_helper.sh")"

[[ -f "$BASE16_SHELL/profile_helper.sh" ]] && base16_zenburn
