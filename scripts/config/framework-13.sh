#!/bin/bash

if is_framework_13; then
  log_step "framework 13"

  log_info "turning off keyboard backlight on boot"
  sudo tee /etc/tmpfiles.d/kbd-backlight.conf >/dev/null <<'EOF'
w /sys/class/leds/chromeos::kbd_backlight/brightness - - - - 0
EOF

  # Hand lid-close suspend to Hyprland (~/.config/hypr/scripts/grace-suspend.sh)
  # so docking within a grace window keeps the dock connection warm. A plain
  # USB-C dock cannot wake this machine from s2idle once suspended, so we delay
  # the suspend instead of fighting the firmware.
  log_info "delegating lid-close suspend to the grace-suspend script"
  sudo tee /etc/systemd/logind.conf.d/10-lid-grace-suspend.conf >/dev/null <<'EOF'
# Managed by dotfiles (scripts/config/framework-13.sh).
[Login]
HandleLidSwitch=ignore
EOF
  sudo systemctl reload systemd-logind
else
  log_info "skipping framework 13 config: not a framework 13"
fi
