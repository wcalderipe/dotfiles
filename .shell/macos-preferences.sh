#!/usr/bin/env bash -x

# Ask for the administrator password upfront
sudo -v

# Save to disk (not to iCloud) by default
defaults write NSGlobalDomain NSDocumentSaveNewDocumentsToCloud -bool false

# Disable the "Are you sure you want to open this application?" dialog
defaults write com.apple.LaunchServices LSQuarantine -bool false

# Never go into computer sleep mode
sudo systemsetup -setcomputersleep Off > /dev/null

# Keyboard {{
  # Disable press-and-hold for keys in favor of key repeat
  defaults write NSGlobalDomain ApplePressAndHoldEnabled -bool false

  # Set a blazingly fast keyboard repeat rate
  defaults write NSGlobalDomain KeyRepeat -int 2

  # Reduce delay
  defaults write NSGlobalDomain InitialKeyRepeat -int 15

  # Map the Caps Lock to Ctrl in the main MacBook keyboard
  # This configuration only will work when you restart your computer
  MAIN_MACBOOK_KEYBOARD_ID="1452-591-0"

  KEYBOARD_MODIFIER_CAPSLOCK_TO_CTRL="<dict>"
  KEYBOARD_MODIFIER_CAPSLOCK_TO_CTRL+="<key>HIDKeyboardModifierMappingDst</key>"
  KEYBOARD_MODIFIER_CAPSLOCK_TO_CTRL+="<integer>2</integer>"
  KEYBOARD_MODIFIER_CAPSLOCK_TO_CTRL+="<key>HIDKeyboardModifierMappingSrc</key>"
  KEYBOARD_MODIFIER_CAPSLOCK_TO_CTRL+="<integer>0</integer>"
  KEYBOARD_MODIFIER_CAPSLOCK_TO_CTRL+="</dict>"

  defaults -currentHost \
    write -g com.apple.keyboard.modifiermapping.${MAIN_MACBOOK_KEYBOARD_ID} \
    -array $KEYBOARD_MODIFIER_CAPSLOCK_TO_CTRL
# }}

# Finder {{
  # Finder: disable window animations and Get Info animations
  defaults write com.apple.finder DisableAllAnimations -bool true

  # Finder: show all filename extensions
  defaults write NSGlobalDomain AppleShowAllExtensions -bool true

  # Finder: show status bar
  defaults write com.apple.finder ShowStatusBar -bool true

  # Finder: show path bar
  defaults write com.apple.finder ShowPathbar -bool true

  # When performing a search, search the current folder by default
  defaults write com.apple.finder FXDefaultSearchScope -string "SCcf"

  # Avoid creating .DS_Store files on network volumes
  defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true

  # Use list view in all Finder windows by default
  # Four-letter codes for the other view modes: `icnv`, `clmv`, `Flwv`
  defaults write com.apple.finder FXPreferredViewStyle -string "Nlsv"

  # Disable the warning before emptying the Trash
  defaults write com.apple.finder WarnOnEmptyTrash -bool false

  # Show the ~/Library folder
  chflags nohidden ~/Library

  # Show the /Volumes folder
  sudo chflags nohidden /Volumes
# }}

# Dock {{
  # Enable highlight hover effect for the grid view of a stack (Dock)
  defaults write com.apple.dock mouse-over-hilite-stack -bool true

  # Set the icon size of Dock items to 36 pixels
  defaults write com.apple.dock tilesize -int 42

  # Automatically hide and show the Dock
  defaults write com.apple.dock autohide -bool true

  # Change minimize/maximize window effect
  defaults write com.apple.dock mineffect -string "genie"

  # Don’t group windows by application in Mission Control
  defaults write com.apple.dock expose-group-by-app -bool false

  # Lower the auto-hiding Dock delay
  defaults write com.apple.dock autohide-delay -float 0.2

  # Remove the animation when hiding/showing the Dock
  defaults write com.apple.dock autohide-time-modifier -int 0

  # Change style to dark mode
  # May need relaunch Dock, use `killall Dock` command
  defaults write NSGlobalDomain AppleInterfaceStyle Dark
# }}

# App Store {{
  # Check for software updates daily, not just once per week
  defaults write com.apple.SoftwareUpdate ScheduleFrequency -int 1

  # Download newly available updates in background
  defaults write com.apple.SoftwareUpdate AutomaticDownload -int 1

  # Install System data files & security updates
  defaults write com.apple.SoftwareUpdate CriticalUpdateInstall -int 1

  # Turn on app auto-update
  defaults write com.apple.commerce AutoUpdate -bool true

  # Do not allow the App Store to reboot machine on macOS updates
  defaults write com.apple.commerce AutoUpdateRestartRequired -bool false
# }}

