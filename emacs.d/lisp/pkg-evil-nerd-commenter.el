;;; pkg-evil-nerd-commenter.el --- -*- lexical-binding: t; -*-

;;; Comment/uncomment lines efficiently. Like Nerd Commenter in Vim.

(require 'use-package)

(use-package evil-nerd-commenter
  :straight t
  :after (evil)
  :commands (evilnc-comment-or-uncomment-lines)
  :init
  ;; Improved toggle comment/uncomment lines.
  (general-define-key
   "M-;" #'evilnc-comment-or-uncomment-lines))

(provide 'pkg-evil-nerd-commenter)

;;; pkg-evil-nerd-commenter.el ends here
