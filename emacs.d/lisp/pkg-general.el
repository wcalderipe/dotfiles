;;; pkg-general.el --- -*- lexical-binding: t; -*-

;;; Modular completion framework.

(require 'use-package)

(use-package general
  :straight t
  :demand t
  :config
  (general-define-key
   ;; Kill the current buffer by default.
   "C-x k" #'kill-this-buffer))

(provide 'pkg-general)

;;; pkg-general.el ends here
