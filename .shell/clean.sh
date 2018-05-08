echo "Removing dotfiles..."
rm -f \
  .gitconfig \
  .tmux.conf \
  .vimrc \
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

echo "Removing vim configurations..."
rm -rf ~/.vim/

echo "Removing config manager at ~/.my-conf..."
rm -rf ~/.my-conf/

echo "DONE!"
