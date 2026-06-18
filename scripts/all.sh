#!/bin/bash
set -euo pipefail

SCRIPTS_DIR="$(cd "$(dirname "$0")" && pwd)"

source "$SCRIPTS_DIR/helpers.sh"

log_step "removing preinstalls"
source "$SCRIPTS_DIR/packaging/remove-preinstalls.sh"

log_step "packaging"
# Symlink farm for managing dotfiles
pkg_install stow
# Database GUI client
pkg_install dbeaver
# Infrastructure as code
pkg_install terraform
# Configuration management and automation
pkg_install ansible
# Use phone as a webcam
pkg_install droidcam
# GTK app to tune webcam controls (focus, exposure, etc.)
pkg_install cameractrls
# Raw photo editor and digital darkroom
pkg_install darktable
# 1Password command-line client
pkg_install 1password-cli
source "$SCRIPTS_DIR/packaging/claude.sh"
source "$SCRIPTS_DIR/packaging/dev-envs.sh"

log_step "config"
source "$SCRIPTS_DIR/config/stow-dotfiles.sh"
source "$SCRIPTS_DIR/config/hyprland-overrides.sh"
source "$SCRIPTS_DIR/config/framework-13.sh"
source "$SCRIPTS_DIR/config/oom-protection.sh"
source "$SCRIPTS_DIR/config/chromium.sh"
