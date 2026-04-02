#!/bin/bash

HYPR_CONF="$HOME/.config/hypr/hyprland.conf"
SOURCE_LINE="source = ~/.config/hypr/overrides.conf"

if [[ ! -f "$HYPR_CONF" ]]; then
  log_warn "hyprland.conf not found, skipping overrides"
  return 0
fi

if grep -qF "$SOURCE_LINE" "$HYPR_CONF"; then
  log_info "hyprland overrides already wired"
else
  echo "$SOURCE_LINE" >> "$HYPR_CONF"
  log_info "hyprland overrides wired"
fi
