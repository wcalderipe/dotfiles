;;; pkg-paredit.el --- -*- lexical-binding: t; -*-

;;; Minor mode for performing structured editing of S-expression data.

(require 'use-package)

(use-package paredit
  :straight t
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
      (add-hook hook #'enable-paredit-mode)))

  ;; Adds to IELM outside the dolist because it doesn't have a mode function.
  (add-hook 'ielm-mode-hook #'enable-paredit-mode))

(provide 'pkg-paredit)

;;; pkg-paredit.el ends here
