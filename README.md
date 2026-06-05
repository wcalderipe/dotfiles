# dotfiles

## Hyprland

### Lid, dock & suspend

Clamshell-docking for Framework 13 (AMD, s2idle). A plain USB-C dock cannot
wake the laptop once it has suspended, so suspend is delayed and the internal
panel is managed on dock. Full rationale lives in the headers of the
scripts under `hypr/.config/hypr/scripts/`.

#### Closing the lid (on battery)

Locks and turns the laptop panel off, then **stays awake for 120s**:

- **Dock within 120s** never suspends; the external monitor shows the lock screen
- **Don't dock** suspends after 120s (battery safe)

On AC, closing the lid starts no timer; it stays awake.

#### Docking

| When | Behavior |
|---|---|
| Dock while lid open (desk, dual monitor) | laptop screen stays on |
| Dock while lid closed (clamshell) | laptop panel disabled, workspaces migrate to the external |
| Close lid while already docked | laptop panel disabled (omarchy), no suspend |
| Undock (lid closed) | laptop panel re-enabled (omarchy) |

Works with any external monitor on any output.

## Keyboard

Layout is plain `us` with **Compose on Right Alt** (`compose:ralt` in
`hypr/.config/hypr/input.conf`; Caps is remapped to Ctrl). Accents are typed as
Compose sequences: tap and release Right Alt, then type the keys in order. It
is not a chord. Because this is `us` and not `us-intl`, the keys `'` `"` `` `
`` `~` still type literally, which keeps them clean for code.

### Portuguese accents

| Want        | Sequence                | Notes              |
|-------------|-------------------------|--------------------|
| ã õ         | `RAlt` `~` vowel      | til                |
| á é í ó ú   | `RAlt` `'` vowel      | agudo              |
| â ê ô       | `RAlt` `^` vowel      | circunflexo (`^` = Shift+6) |
| à           | `RAlt` `` ` `` `a`      | crase              |
| ç           | `RAlt` `,` `c`          | cedilha            |

Uppercase: add Shift on the letter. `RAlt` `,` `Shift+C` → Ç.

### French and Spanish

The same Compose key covers them:

| Want      | Sequence                            | Notes             |
|-----------|-------------------------------------|-------------------|
| ñ         | `RAlt` `~` `n`                       | español           |
| ü ë ï     | `RAlt` `"` vowel                   | trema / tréma     |
| è ù       | `RAlt` `` ` `` vowel               | accent grave      |
| ¿ ¡       | `RAlt` `?` `?`  /  `RAlt` `!` `!`    | español           |

