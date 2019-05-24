(add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
(add-hook 'clojure-mode-hook 'smartparens-mode)
(add-hook 'clojurescript-mode-hook 'smartparens-mode)

(smartparens-global-strict-mode 1)

;; Do not generate a pair of single quotes or backticks unless the
;; point is inside comments/strings.
(let ((modes (list 'emacs-lisp-mode 'clojure-mode 'clojurescript-mode)))
  (sp-local-pair modes "`" nil :when '(sp-in-string-p sp-in-comment-p))
  (sp-local-pair modes "'" nil :when '(sp-in-string-p sp-in-comment-p)))
