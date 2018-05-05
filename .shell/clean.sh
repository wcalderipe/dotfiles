echo "Removing dotfiles..."
rm -f \
  .zshrc \
  .tmux-conf \
  README.md

echo "Removing scripts..."
rm -rf ~/.shell/

echo "Removing config manager at ~/.my-conf..."
rm -rf ~/.my-conf

echo "DONE!"
