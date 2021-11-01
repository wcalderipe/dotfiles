;;; pkg-git-link.el --- -*- lexical-binding: t; -*-

;;; Helpful package to quick share fragments of code with your teammates.

(require 'use-package)

(use-package git-link
  :straight t
  :defer t
  :config
  ;; This might create a reference in the wrong lines if you're using branches
  ;; for development and your current version is too far ahead from origin.
  (setq git-link-default-branch "master"))

(provide 'pkg-git-link)

;;; pkg-git-link.el ends here
