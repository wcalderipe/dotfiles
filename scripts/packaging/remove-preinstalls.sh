#!/bin/bash

pkg_remove alacritty
pkg_remove spotify

log_step "removing webapps"
omarchy-webapp-remove \
  "HEY" \
  "Basecamp" \
  "WhatsApp" \
  "Google Photos" \
  "Google Contacts" \
  "Google Messages" \
  "Google Maps" \
  "ChatGPT" \
  "YouTube" \
  "X" \
  "Fizzy"

log_step "removing npx globals"
rm -f ~/.local/bin/codex
rm -f ~/.local/bin/gemini
rm -f ~/.local/bin/copilot
