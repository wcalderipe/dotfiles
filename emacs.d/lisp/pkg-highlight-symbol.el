;;; pkg-highlight-symbol.el --- -*- lexical-binding: t; -*-

;;; Toggle highlighting of the symbol at point.

(require 'use-package)

(use-package highlight-symbol
  :straight t
  :defer t
  :hook ((emacs-lisp-mode clojure-mode clojurescript-mode) . highlight-symbol-mode)
  :init
  ;; Reduce default idle delay of 1.5s.
  (setq highlight-symbol-idle-delay 0.5))

(provide 'pkg-highlight-symbol)

;;; pkg-highlight-symbol.el ends here
