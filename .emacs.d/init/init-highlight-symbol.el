;; Reduce default idle delay of 1.5s
(setq highlight-symbol-idle-delay 0.5)

;; Enable minor mode only for Elisp and Clojure major modes.
(add-hook 'emacs-lisp-mode-hook 'highlight-symbol-mode)
(add-hook 'clojure-mode-hook 'highlight-symbol-mode)
