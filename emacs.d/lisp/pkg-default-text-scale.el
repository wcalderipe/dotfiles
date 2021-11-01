;;; pkg-default-text-scale.el --- -*- lexical-binding: t; -*-

;;; Easily adjust the font size in all Emacs frames.

(require 'use-package)

(use-package default-text-scale
  :when my/gui?
  :straight t
  :defer t
  :init
  (general-define-key
   "M-=" #'default-text-scale-increase
   "M--" #'default-text-scale-decrease
   "M-0" #'default-text-scale-reset))

(provide 'pkg-default-text-scale)

;;; pkg-default-text-scale.el ends here
