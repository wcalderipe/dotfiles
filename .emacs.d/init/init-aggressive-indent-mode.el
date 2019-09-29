;; Only enable this mode for Lisps. I found this mode too distracting
;; when developing with Javascript.
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)

;; You can disable aggressive indent mode for specific packages.
;; (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
