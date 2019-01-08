# Dotfiles

## Linux (Ubuntu)

```bash
# If you're in a fresh Linux machine
sudo apt-get update && apt-get install --yes curl git

# Clean up to start from scratch (optional step)
curl -Lks https://raw.githubusercontent.com/wcalderipe/dotfiles/master/.shell/clean.sh | /bin/bash

# Install dotfiles
curl -Lks https://raw.githubusercontent.com/wcalderipe/dotfiles/master/.shell/setup.sh | /bin/bash
```

## MacOS

```bash
# Install brew formulas (optional step)
# If you're in a fresh MacOS machine you must run this scripts before macos-setup.sh 
curl -Lks https://raw.githubusercontent.com/wcalderipe/dotfiles/master/.shell/brew.sh | /bin/bash

# Clean up to start from scratch (optional step)
curl -Lks https://raw.githubusercontent.com/wcalderipe/dotfiles/master/.shell/clean.sh | /bin/bash

# Install dotfiles
curl -Lks https://raw.githubusercontent.com/wcalderipe/dotfiles/master/.shell/setup.sh | /bin/bash
```

## Vim

### TypeScript

| Mode   | Command       | Description                                                           |
|--------|---------------|-----------------------------------------------------------------------|
| insert | `<C-x> <C-o>` | Show completions                                                      |
| normal | `<C-]>`       | Navigate to location where the symbol under the cursos is defined     |
| normal | `<C-t>`       | Navigate the cursor back after `<C-]>`                                |
| normal | `<C-^>`       | Show list of location where the symbol under the cursos is referenced |

## References

- https://github.com/ilmotta/dotfiles
