(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . web-mode))

(add-hook 'typescript-mode-hook #'my/typescript-mode)

(add-hook 'web-mode-hook (lambda ()
                           (when (string-equal "tsx" (file-name-extension buffer-file-name))
                             (my/typescript-mode))))

(flycheck-add-mode 'typescript-tslint 'web-mode)
