;;; pkg-clojure-mode.el --- -*- lexical-binding: t; -*-

(require 'use-package)

(use-package clojure-mode
  :straight t
  :defer t
  :hook (outline-minor-mode . clojure-mode)
  :config
  (with-eval-after-load 'flycheck
    (require 'flycheck-clj-kondo nil 'noerror))

  (general-define-key
   :prefix my/mode-leader
   :states 'normal
   :keymaps 'clojure-mode-map
   ;; Code
   "c n s" #'clojure-sort-ns))

(provide 'pkg-clojure-mode)

;;; pkg-clojure-mode.el ends here
