echo "Removing dotfiles..."
rm -f \
  .tmux.conf \
  .zpreztorc \
  .zshrc \
  README.md

echo "Removing scripts..."
rm -rf ~/.shell/

echo "Uninstalling Tmux..."
rm -rf ~/.tmux/

echo "Removing zprezto..."
rm -rf ~/.zprezto/

echo "Removing config manager at ~/.my-conf..."
rm -rf ~/.my-conf

echo "DONE!"
