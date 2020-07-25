#!/usr/bin/env sh

[[ -f ~/.zprezto/init.zsh ]] && source ~/.zprezto/init.zsh

[[ -f ~/.fzf.zsh ]] && source ~/.fzf.zsh

# Enable Ctrl-R for navigating through the history.
bindkey "^R" history-incremental-search-backward

# Base16 shell
BASE16_SHELL="$HOME/.config/base16-shell/"

[ -n "$PS1" ] && \
    [ -s "$BASE16_SHELL/profile_helper.sh" ] && \
        eval "$("$BASE16_SHELL/profile_helper.sh")"

[[ -f "$BASE16_SHELL/profile_helper.sh" ]] && base16_zenburn


# Dependencies required variables (e.g. rbenv, jenv, yarn).
# This file is created during the Ansible playbook and edited by
# tasks, therefore it shouldn't be versioned.
[[ -f ~/.composed-variables ]] && source ~/.composed-variables

[[ -f ~/vars ]] && source ~/vars
[[ -f ~/aliases ]] && source ~/aliases
[[ -f ~/private-variables ]] && source ~/private-variables
