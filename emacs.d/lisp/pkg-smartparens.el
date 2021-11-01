;;; pkg-smartparens.el --- -*- lexical-binding: t; -*-

;;; A minor mode for dealing with pairs in Emacs.

(require 'use-package)

(use-package smartparens
  :straight t
  :defer t
  :init
  ;; Only hooks with Lisp family major modes.
  (dolist (mode '(emacs-lisp-mode
                  common-lisp-mode
                  lisp-mode
                  clojure-mode
                  clojurec-mode
                  clojurescript-mode
                  cider-repl-mode))
    (let ((hook (intern (concat (symbol-name mode) "-hook"))))
      (add-hook hook #'smartparens-mode)))

  ;; Adds to IELM outside the dolist because it doesn't have a mode function.
  (add-hook 'ielm-mode-hook #'smartparens-mode))

(provide 'pkg-smartparens)

;;; pkg-smartparens.el ends here
