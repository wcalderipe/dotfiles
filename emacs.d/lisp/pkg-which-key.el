;;; pkg-which-key.el --- -*- lexical-binding: t; -*-

;;; Displays available keybindings (discoverability).

(require 'use-package)

(use-package which-key
  :straight t
  :config
  (which-key-setup-minibuffer)
  (which-key-mode))

(provide 'pkg-which-key)

;;; pkg-which-key.el ends here
