;;; pkg-clojure-mode.el --- -*- lexical-binding: t; -*-

(require 'use-package)

(use-package clojure-mode
  :straight t
  :defer t
  :hook (outline-minor-mode . clojure-mode)
  :config
  (with-eval-after-load 'flycheck
    (require 'flycheck-clj-kondo nil 'noerror))

  (with-eval-after-load 'smartparens
    (require 'smartparens-clojure))

  (general-define-key
   :prefix my/mode-leader
   :states 'normal
   :keymaps 'clojure-mode-map
   "M-a" #'sp-forward-sexp
   ;; Namespace
   "n s" #'clojure-sort-ns
   "n i" #'clojure-insert-ns-form
   ;; Code
   "c a" #'clojure-align))

(provide 'pkg-clojure-mode)

;;; pkg-clojure-mode.el ends here
