echo "Removing dotfiles..."
rm -f \
  .tmux.conf \
  .zpreztorc \
  .zshrc \
  README.md

echo "Removing tmux..."
rm -rf ~/.tmux/

echo "Removing scripts..."
rm -rf ~/.shell/

echo "Removing zprezto..."
rm -rf ~/.zprezto/

echo "Removing iTerm configurations..."
rm -rf ~/.iterm/

echo "Removing config manager at ~/.my-conf..."
rm -rf ~/.my-conf/

echo "Removing vimfiles..."
rm -rf ~/.vim/

echo "Removing nvim configurations..."
rm -rf ~/.config/nvim

echo "DONE!"
