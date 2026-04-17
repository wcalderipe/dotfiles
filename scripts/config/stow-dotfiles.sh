#!/bin/bash

log_info "stowing: hypr"
stow -d "$SCRIPTS_DIR/.." -t "$HOME" hypr

log_info "stowing: claude"
stow -d "$SCRIPTS_DIR/.." -t "$HOME" claude
