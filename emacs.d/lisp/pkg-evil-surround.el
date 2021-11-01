;;; pkg-evil-surround.el --- -*- lexical-binding: t; -*-

;;; Emulates Surround.vim for Evil. Everything about "surroundings":
;;; parentheses, brackets, quotes, XML tags, and more.

(require 'use-package)

(use-package evil-surround
  :straight t
  :defer t
  :after (evil)
  :config
  (add-to-list 'evil-surround-operator-alist '(evil-cp-change . change))
  (add-to-list 'evil-surround-operator-alist '(evil-cp-delete . delete))
  :init
  (add-hook 'prog-mode-hook #'global-evil-surround-mode))

(provide 'pkg-evil-surround)

;;; pkg-evil-surround.el ends here
