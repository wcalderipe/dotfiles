;;; pkg-org-evil.el --- -*- lexical-binding: t; -*-

;;; Supplemental evil-mode key-bindings to org-mode.

(require 'use-package)

(use-package evil-org
  :straight t
  :after (org evil)
  :commands (evil-org evil-org-agenda)
  :init (add-hook 'org-mode-hook 'evil-org-mode))

(provide 'pkg-org-evil)

;;; pkg-org-evil.el ends here
