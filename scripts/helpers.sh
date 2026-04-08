#!/bin/bash

BOLD='\033[1m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
RED='\033[0;31m'
RESET='\033[0m'

log_info()  { echo -e "${GREEN}[ok]${RESET} $1"; }
log_warn()  { echo -e "${YELLOW}[warn]${RESET} $1"; }
log_error() { echo -e "${RED}[error]${RESET} $1"; }
log_step()  { echo -e "\n${BOLD}--- $1 ---${RESET}"; }

is_framework_13() {
  grep -q "Laptop 13" /sys/devices/virtual/dmi/id/product_name 2>/dev/null
}

pkg_install() {
  log_info "installing: $*"
  yay -S --noconfirm --needed "$@"
}

pkg_remove() {
  local pkg
  for pkg in "$@"; do
    if pacman -Q "$pkg" &>/dev/null; then
      log_info "removing: $pkg"
      sudo pacman -Rns --noconfirm "$pkg"
    else
      log_info "already removed: $pkg"
    fi
  done
}
