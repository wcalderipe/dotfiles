;; Add web-mode to .tsx files
(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . web-mode))

;; Spin tide server and set flycheck-mode options
(add-hook 'typescript-mode-hook #'my/typescript-mode)

(add-hook 'web-mode-hook (lambda ()
                           (when (string-equal "tsx" (file-name-extension buffer-file-name))
                             (my/set-local-indent 2)
                             ;; Indent using spaces
                             (setq-local indent-tabs-mode nil)
                             (my/typescript-mode))))

(flycheck-add-mode 'typescript-tslint 'web-mode)
