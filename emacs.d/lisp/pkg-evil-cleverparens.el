;;; pkg-evil-cleverparens.el --- -*- lexical-binding: t; -*-

(require 'use-package)

(use-package evil-cleverparens
  :straight t
  :config
  (dolist (mode '(emacs-lisp-mode
                  common-lisp-mode
                  lisp-mode
                  clojure-mode
                  clojurec-mode
                  clojurescript-mode
                  cider-repl-mode))
    (let ((hook (intern (concat (symbol-name mode) "-hook"))))
      (add-hook hook #'evil-cleverparens-mode))))

(provide 'pkg-evil-cleverparens)

;;; pkg-evil-cleverparens.el ends here
