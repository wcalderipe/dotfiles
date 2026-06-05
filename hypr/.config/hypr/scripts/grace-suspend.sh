#!/usr/bin/env bash
#
# grace-suspend.sh — delay suspend after lid close so docking stays "warm".
#
# Why this exists
# ---------------
# This Framework AMD laptop suspends via s2idle and will NOT wake when a plain
# USB-C dock is hot-plugged into an already-suspended machine (firmware limit,
# verified empirically: cold dock-connect never re-enumerates or wakes). So the
# old flow — close lid (suspend) → dock → nothing happens — cannot work.
#
# Instead, lid close locks the session and turns the internal panel off but
# stays awake for a grace window. If you dock within that window the laptop is
# still awake, the external monitor enumerates normally and shows the lock
# screen, and it never suspends (now on AC). If you don't dock, it suspends as
# usual and battery is safe.
#
# Wiring (see ~/.config/hypr/overrides.conf):
#   bindl = , switch:on:Lid Switch,  exec, ~/.config/hypr/scripts/grace-suspend.sh arm
#   bindl = , switch:off:Lid Switch, exec, ~/.config/hypr/scripts/grace-suspend.sh disarm
#
# Requires logind HandleLidSwitch=ignore (scripts/config/framework-13.sh) so
# logind does not suspend immediately and race this script.
#
# Note: these lid hooks are ADDITIVE to omarchy's own lid bindings. omarchy's
# switch:on handler only disables the internal display (and only when an
# external monitor is present); this script never touches that, so it does not
# recreate the double-disable-eDP bug noted in hyprland.conf.

set -euo pipefail

GRACE_SECONDS=120
UNIT="lid-grace-suspend"

ac_online() {
  local f
  for f in /sys/class/power_supply/A*/online; do
    if [[ -r "$f" && "$(<"$f")" == "1" ]]; then
      return 0
    fi
  done
  return 1
}

lid_closed() {
  grep -qiw closed /proc/acpi/button/lid/*/state 2>/dev/null
}

# Self-contained external-monitor check (mirrors omarchy-hw-external-monitors).
# Inlined so it works inside the systemd --user unit, whose PATH does not
# include omarchy's bin directory.
external_connected() {
  local s
  for s in /sys/class/drm/card*-*/status; do
    [[ "$s" == *-eDP-*/status ]] && continue
    if [[ -r "$s" && "$(<"$s")" == "connected" ]]; then
      return 0
    fi
  done
  return 1
}

internal_display() {
  hyprctl monitors -j 2>/dev/null | jq -r '.[] | select(.name | contains("eDP")).name' | head -n1
}

cancel_timer() {
  systemctl --user stop "$UNIT.timer" 2>/dev/null || true
  systemctl --user reset-failed "$UNIT.service" 2>/dev/null || true
}

case "${1:-}" in
  arm)
    # Already on power (e.g. docked): never suspend, let omarchy handle the
    # display and stay awake. Nothing to schedule.
    if ac_online; then exit 0; fi

    # Schedule the conditional suspend FIRST so it is guaranteed even if the
    # lock/dpms steps below misbehave.
    cancel_timer
    systemd-run --user --quiet \
      --on-active="$GRACE_SECONDS" --unit="$UNIT" \
      --description="Suspend after lid-close grace window" \
      "$HOME/.config/hypr/scripts/grace-suspend.sh" fire

    # Lock now (lock-only: do not let omarchy schedule its own display-off).
    OMARCHY_LOCK_ONLY=true omarchy-system-lock || true

    # Turn the (closed) laptop panel off to save power during the window.
    dpy="$(internal_display || true)"
    if [[ -n "${dpy:-}" ]]; then
      hyprctl dispatch dpms off "$dpy" || true
    fi
    ;;

  fire)
    # Reached only if the grace window elapsed. Suspend only if still closed,
    # still on battery, and no external monitor showed up (i.e. not docked).
    if ! lid_closed; then exit 0; fi
    if ac_online; then exit 0; fi
    if external_connected; then exit 0; fi
    systemctl suspend
    ;;

  disarm)
    # Lid opened: cancel the pending suspend and turn the panel back on.
    cancel_timer
    dpy="$(internal_display || true)"
    if [[ -n "${dpy:-}" ]]; then
      hyprctl dispatch dpms on "$dpy" || true
    fi
    ;;

  *)
    echo "usage: $(basename "$0") {arm|fire|disarm}" >&2
    exit 1
    ;;
esac
