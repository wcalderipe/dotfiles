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

## References

- https://github.com/ilmotta/dotfiles
