;;; pkg-flycheck.el --- -*- lexical-binding: t; -*-

;;; On-the-fly syntax checking.

(require 'use-package)

(use-package flycheck
  :straight t
  :ensure t
  :defer t
  :init (global-flycheck-mode))

(provide 'pkg-flycheck)

;;; pkg-flycheck.el ends here
