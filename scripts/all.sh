#!/bin/bash
set -euo pipefail

SCRIPTS_DIR="$(cd "$(dirname "$0")" && pwd)"

source "$SCRIPTS_DIR/helpers.sh"

log_step "removing preinstalls"
source "$SCRIPTS_DIR/packaging/remove-preinstalls.sh"

log_step "packaging"
pkg_install stow
pkg_install dbeaver
pkg_install terraform
pkg_install ansible
pkg_install droidcam
pkg_install darktable
pkg_install 1password-cli
source "$SCRIPTS_DIR/packaging/dev-envs.sh"
source "$SCRIPTS_DIR/packaging/claude-code.sh"

log_step "config"
source "$SCRIPTS_DIR/config/stow-dotfiles.sh"
source "$SCRIPTS_DIR/config/hyprland-overrides.sh"
