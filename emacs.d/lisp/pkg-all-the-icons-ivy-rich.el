;;; pkg-all-the-icons-ivy-rich.el --- -*- lexical-binding: t; -*-

;;; Displays icons for all buffers in Ivy.

(require 'use-package)

(use-package all-the-icons-ivy-rich
  :when my/gui?
  :straight t
  :init (all-the-icons-ivy-rich-mode 1))

(provide 'pkg-all-the-icons-ivy-rich)

;;; pkg-all-the-icons-ivy-rich.el ends here
