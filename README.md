# dotfiles

## Hyprland

### Lid, dock & suspend

Clamshell-docking for Framework 13 (AMD, s2idle). A plain USB-C dock cannot
wake the laptop once it has suspended, so suspend is delayed and the internal
panel is managed on dock. Full rationale lives in the headers of the
scripts under `hypr/.config/hypr/scripts/`.

#### Closing the lid (on battery)

Locks and turns the laptop panel off, then **stays awake for 120s**:

- **Dock within 120s** → never suspends; the external monitor shows the lock screen.
- **Don't dock** → suspends after 120s (battery safe).

On AC, closing the lid starts no timer — it stays awake.

#### Docking

| Situation | Behavior |
|---|---|
| Dock while lid **open** (desk, dual monitor) | laptop screen stays on |
| **Dock while lid closed** (clamshell) | laptop panel disabled, workspaces migrate to the external |
| Close lid while already docked | laptop panel disabled (omarchy), no suspend |
| Undock (lid closed) | laptop panel re-enabled (omarchy) |

Works with any external monitor on any output — nothing is tied to a specific
dock or display.
