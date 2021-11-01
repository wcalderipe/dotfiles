;;; pkg-lsp-mode.el --- -*- lexical-binding: t; -*-

(require 'use-package)

(use-package lsp-mode
  :straight t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t)
  :commands (lsp lsp-deferred))

(provide 'pkg-lsp-mode)

;;; pkg-lsp-mode.el ends here
