;;; pkg-org-cliplink.el --- -*- lexical-binding: t; -*-

;;; A simple command that takes a URL from the clipboard and inserts an org-mode
;;; link with a title already.

(require 'use-package)

(use-package org-cliplink
  :straight t
  :defer t)

(provide 'pkg-org-cliplink)

;;; pkg-org-cliplink.el ends here
