;; Dim the colors of other data structure literals.
(add-hook 'clojure-mode-hook (lambda () (setq paren-face-regexp "#?[](){}[]")))

(custom-set-faces
 '(parenthesis ((t (:foreground "gray43" :underline nil :weight normal)))))

(global-paren-face-mode)
