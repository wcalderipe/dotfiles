;;; pkg-restart-emacs.el --- -*- lexical-binding: t; -*-

;;; Modular completion framework.

(require 'use-package)

(use-package restart-emacs
  :straight t
  :commands (restart-emacs)
  :init
  (general-define-key
   ;; TODO: general is not setting the key binding for some reason.
   "C-x C-r" #'restart-emacs))

(provide 'pkg-restart-emacs)

;;; pkg-restart-emacs.el ends here
