;; This mode is used, for example, to replace the word "lambda" with
;; the unicode character λ.
(add-hook 'emacs-lisp-mode-hook 'global-prettify-symbols-mode)

;; Enable evaluation in an overlay.
(add-hook 'emacs-lisp-mode-hook
          (lambda () (eros-mode 1)))

(add-hook 'emacs-lisp-mode-hook
          (lambda () (setq prettify-symbols-alist
                      '(
                        ("lambda" . ?λ)
                        ))))
