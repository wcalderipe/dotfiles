;;; pkg-editorconfig.el --- -*- lexical-binding: t; -*-

;;; Editorconfig plugin for Emacs.

(require 'use-package)

(use-package editorconfig
  :straight t
  :defer t
  :hook (prog-mode . editorconfig-mode))

(provide 'pkg-editorconfig)

;;; pkg-editorconfig.el ends here
