#!/bin/bash

log_info "stowing: hypr"
stow -d "$SCRIPTS_DIR/.." -t "$HOME" hypr
