#!/usr/bin/env bash
#
# dock-internal-off.sh — disable the internal panel when an external monitor is
# connected while the laptop lid is closed.
#
# Why this exists
# ---------------
# omarchy disables the internal display on the lid-switch event (and only when
# an external monitor is already present). But when you dock with the lid
# ALREADY closed, no lid event fires, so the internal panel stays an active —
# but invisible — monitor, stranding its workspaces on the shut-lid screen.
# This watches Hyprland's event socket and closes that gap. It is the add-side
# complement to omarchy-hyprland-monitor-watch (which only handles removal).
#
# Guarded by lid state so plugging a second monitor while the lid is OPEN (a
# normal multi-monitor desk setup) does not disable the laptop screen.
#
# Started from ~/.config/hypr/overrides.conf:
#   exec-once = uwsm-app -- ~/.config/hypr/scripts/dock-internal-off.sh

set -euo pipefail

SOCKET="$XDG_RUNTIME_DIR/hypr/$HYPRLAND_INSTANCE_SIGNATURE/.socket2.sock"

lid_closed() {
  grep -qiw closed /proc/acpi/button/lid/*/state 2>/dev/null
}

socat -U - "UNIX-CONNECT:$SOCKET" | while read -r event; do
  case "$event" in
    monitoradded\>\>*|monitoraddedv2\>\>*)
      if lid_closed; then
        omarchy-hyprland-monitor-internal off || true
      fi
      ;;
  esac
done
