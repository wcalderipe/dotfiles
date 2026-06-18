#!/bin/bash

log_step "chromium memory saver"

# Chromium reads enterprise policy JSON from /etc/chromium/policies/managed/.
# Memory Saver kept switching itself off; a managed policy pins it on and locks
# the UI toggle, so a profile reset, an update, or a stray click cannot undo it.
#   HighEfficiencyModeEnabled  enable Memory Saver (discard idle background tabs)
#   MemorySaverModeSavings     0 Moderate, 1 Balanced, 2 Maximum
log_info "pinning chromium memory saver to balanced"
sudo install -d /etc/chromium/policies/managed
sudo tee /etc/chromium/policies/managed/memory-saver.json >/dev/null <<'EOF'
{
  "HighEfficiencyModeEnabled": true,
  "MemorySaverModeSavings": 1
}
EOF
