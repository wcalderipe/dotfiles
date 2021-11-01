;;; pkg-visual-fill-column.el --- -*- lexical-binding: t; -*-

;;; Instead of wrapping lines at the window edge, which is the standard behaviour
;;; of visual-line-mode, it wraps lines at fill-column.

(require 'use-package)

(use-package visual-fill-column
  :straight t
  :defer t)

(provide 'pkg-visual-fill-column)

;;; pkg-visual-fill-column.el ends here
