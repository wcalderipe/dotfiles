#!/bin/bash
set -euo pipefail

SCRIPTS_DIR="$(cd "$(dirname "$0")" && pwd)"

source "$SCRIPTS_DIR/helpers.sh"

log_step "removing preinstalls"
source "$SCRIPTS_DIR/packaging/remove-preinstalls.sh"

log_step "packaging"
source "$SCRIPTS_DIR/packaging/stow.sh"
source "$SCRIPTS_DIR/packaging/dbeaver.sh"

log_step "config"
source "$SCRIPTS_DIR/config/stow-dotfiles.sh"
source "$SCRIPTS_DIR/config/hyprland-overrides.sh"
