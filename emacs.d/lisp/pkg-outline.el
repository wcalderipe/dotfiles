;;; pkg-outline.el --- -*- lexical-binding: t; -*-

(require 'use-package)

(use-package outline
  :straight (:type built-in)
  :defer t
  :preface
  ;; Support Clojure's comment headings convention.
  ;; See https://github.com/bbatsov/clojure-style-guide#four-semicolons-for-heading-comments
  (defun pkg-clojure-mode/outline-minor-mode-h ()
    (when (derived-mode-p 'clojure-mode)
      (setq-local outline-regexp "[ \t]*;;;;* [^ \t\n]")))

  :config
  (general-define-key
   :keymaps 'outline-minor-mode-map
   :states 'normal
   :predicate '(outline-on-heading-p)
   "C-j"       #'outline-forward-same-level
   "C-k"       #'outline-backward-same-level
   "<tab>"     #'outline-toggle-children
   "<backtab>" #'outline-cycle-buffer))

(provide 'pkg-outline)

;;; pkg-outline.el ends here
