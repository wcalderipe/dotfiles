#!/bin/bash

log_step "oom protection"

# The kernel's built-in OOM killer fires far too late: under memory pressure it
# evicts executable pages and thrashes the disk for minutes before killing
# anything, which presents as a full freeze needing a hard reset. earlyoom
# watches MemAvailable + swap and kills the fattest process early (a leaking
# Chromium renderer wins on oom_score), so the desktop stays responsive.
pkg_install earlyoom

log_info "configuring earlyoom thresholds"
# -m 10,5  SIGTERM at 10% RAM available, SIGKILL at 5%
# -s 10,5  same thresholds for swap (zram)
# -r 3600  report memory state hourly, not every minute
# -n       desktop notification naming what got killed (our root-cause trail)
# --avoid  never kill the compositor, the remote-access lifeline, or an active
#          recording/edit (a killed OBS recording is unrecoverable)
sudo tee /etc/default/earlyoom >/dev/null <<'EOF'
# Managed by dotfiles (scripts/config/oom-protection.sh).
EARLYOOM_ARGS="-m 10,5 -s 10,5 -r 3600 -n --avoid ^(Hyprland|sshd|systemd|systemd-logind|obs)$"
EOF

log_info "enabling earlyoom service"
sudo systemctl enable --now earlyoom.service
sudo systemctl restart earlyoom.service
