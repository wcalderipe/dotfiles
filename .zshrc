#!/usr/bin/env sh

source ~/.zprezto/init.zsh

[[ -f ~/.shell/variables ]] && source ~/.shell/variables
[[ -f ~/.shell/aliases ]] && source ~/.shell/aliases

# Enable Ctrl-R for navigating through the history.
bindkey "^R" history-incremental-search-backward
