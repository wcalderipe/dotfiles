;;; pkg-all-the-icons-dired.el --- -*- lexical-binding: t; -*-

;;; Adds dired support to all-the-icons.

(require 'use-package)

(use-package all-the-icons-dired
  :when my/gui?
  :straight t
  :defer t
  :hook (dired-mode . all-the-icons-dired-mode))

(provide 'pkg-all-the-icons-dired)

;;; pkg-all-the-icons-dired.el ends here
