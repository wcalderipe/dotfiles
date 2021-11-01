;;; pkg-xclip.el --- -*- lexical-binding: t; -*-

;;; Allows Emacs to copy and to paste from the system clipboard.

(require 'use-package)

(use-package xclip
  :straight t
  :defer t
  :config (xclip-mode t))

(provide 'pkg-xclip)

;;; pkg-xclip.el ends here
