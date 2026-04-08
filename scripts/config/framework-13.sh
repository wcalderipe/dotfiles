#!/bin/bash

if ! is_framework_13; then
  log_info "skipping framework 13 config: not a framework 13"
  return 0
fi

log_step "framework 13"

log_info "turning off keyboard backlight on boot"
sudo tee /etc/tmpfiles.d/kbd-backlight.conf >/dev/null <<'EOF'
w /sys/class/leds/chromeos::kbd_backlight/brightness - - - - 0
EOF
