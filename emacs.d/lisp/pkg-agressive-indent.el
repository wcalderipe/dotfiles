;;; pkg-agressive-indent.el --- -*- lexical-binding: t; -*-

(require 'use-package)

(use-package aggressive-indent
  :straight t
  :defer t
  :init
  (dolist (mode '(emacs-lisp-mode
                  common-lisp-mode
                  lisp-mode
                  clojure-mode
                  clojurec-mode
                  clojurescript-mode
                  cider-repl-mode))
    (let ((hook (intern (concat (symbol-name mode) "-hook"))))
      (add-hook hook #'aggressive-indent-mode))))

(provide 'pkg-agressive-indent)

;;; pkg-agressive-indent.el ends here
