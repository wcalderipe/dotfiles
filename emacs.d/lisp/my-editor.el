(use-package zenburn-theme
  :straight t

  :defer t)

;; A minimal and modern mode-line.
(use-package doom-modeline
  :straight t

  :defer t

  :hook (after-init . my/init-load-theme)

  :preface
  ;; I'm not happy on where custom-set-faces are being set inside of
  ;; doom-modeline.
  (defun my/init-load-theme ()
    (custom-set-faces
     `(ivy-current-match              ((t :background ,my/color-gray :foreground nil :underline unspecified :weight unspecified)))
     `(ivy-highlight-face             ((t :background nil :foreground nil :underline unspecified :weight unspecified)))
     `(ivy-minibuffer-match-face-1    ((t :background nil :inherit bold)))
     `(ivy-minibuffer-match-face-2    ((t :background nil :foreground ,my/color-cyan :underline t)))
     `(ivy-minibuffer-match-face-3    ((t :background nil :foreground ,my/color-cyan :underline t)))
     `(ivy-minibuffer-match-face-4    ((t :background nil :foreground ,my/color-cyan :underline t)))
     `(ivy-minibuffer-match-highlight ((t :background ,my/color-gray :foreground nil :underline unspecified :weight unspecified)))
     `(ivy-subdir                     ((t :background nil :underline unspecified :weight unspecified))))

    (doom-modeline-mode +1)
    (load-theme 'zenburn t nil))

  :config
  (doom-modeline-mode 1))


(provide 'my-editor)
