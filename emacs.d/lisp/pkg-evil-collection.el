;;; pkg-evil-collection.el --- -*- lexical-binding: t; -*-

;;; Add evil bindings beyond the default like calendar and help-mode.

(require 'use-package)

(use-package evil-collection
  :straight t
  :after (evil)
  :config (evil-collection-init))

(provide 'pkg-evil-collection)

;;; pkg-evil-collection.el ends here
