# Dotfiles


## MacOS Setup

```bash
# Install brew formulas (optional step)
curl -Lks https://raw.githubusercontent.com/wcalderipe/dotfiles/master/.shell/brew.sh | /bin/bash

# Clean up to start from scratch (optional step)
curl -Lks https://raw.githubusercontent.com/wcalderipe/dotfiles/master/.shell/clean.sh | /bin/bash

# Install dotfiles
curl -Lks https://raw.githubusercontent.com/wcalderipe/dotfiles/master/.shell/macos-setup.sh | /bin/bash
```

## Usage

Use the conf alias to manage all configuration files with git.

```bash
conf status
conf add .gitconfig
conf commit -m 'Add gitconfig'
conf push origin master
```

## References

- https://github.com/ilmotta/dotfiles
