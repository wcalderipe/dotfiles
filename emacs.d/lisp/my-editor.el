(use-package zenburn-theme
  :straight t

  :defer t)

;; A minimal and modern mode-line.
(use-package doom-modeline
  :straight t

  :defer t

  :hook (after-init . my/init-load-theme)

  :preface
  (defun my/init-load-theme ()
    (doom-modeline-mode +1)
    (load-theme 'zenburn t nil))

  :config
  (doom-modeline-mode 1))

(provide 'my-editor)
