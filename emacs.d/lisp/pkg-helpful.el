;;; pkg-helpful.el --- -*- lexical-binding: t; -*-

;;; An alternative to the built-in Emacs help that provides much more
;;; contextual information.

(require 'use-package)

(use-package helpful
  :straight t
  :commands (helpful--read-symbol)
  :init
  (general-define-key
   [remap describe-key] #'helpful-key
   "C-h ." #'helpful-at-point))

(provide 'pkg-helpful)

;;; pkg-helpful.el ends here
