;;; pkg-doom-modeline.el --- -*- lexical-binding: t; -*-

;;; A minimal and modern mode-line.

(require 'use-package)

(use-package doom-modeline
  :straight t
  :defer t
  :hook (after-init . pkg-doom-modeline/init-load-theme)
  :preface
  ;; I'm not happy on where custom-set-faces are being set inside of
  ;; doom-modeline.
  (defun pkg-doom-modeline/init-load-theme ()
    (doom-modeline-mode +1)
    (load-theme 'zenburn t nil)

    (with-eval-after-load 'ivy
      (custom-theme-set-faces
       'zenburn
       `(ivy-current-match              ((t :background ,my/color-gray :foreground nil :underline unspecified :weight unspecified)))
       `(ivy-highlight-face             ((t :background nil :foreground nil :underline unspecified :weight unspecified)))
       `(ivy-minibuffer-match-face-1    ((t :background nil :inherit bold)))
       `(ivy-minibuffer-match-face-2    ((t :background nil :foreground ,my/color-cyan :underline t)))
       `(ivy-minibuffer-match-face-3    ((t :background nil :foreground ,my/color-cyan :underline t)))
       `(ivy-minibuffer-match-face-4    ((t :background nil :foreground ,my/color-cyan :underline t)))
       `(ivy-minibuffer-match-highlight ((t :background ,my/color-gray :foreground nil :underline unspecified :weight unspecified)))
       `(ivy-subdir                     ((t :background nil :underline unspecified :weight unspecified)))))

    (custom-theme-set-faces
     'zenburn
     ;; Removes the annoying secondary color in the buffer divider --
     ;; called fringe.
     `(fringe ((t (:background "#3F3F3F"))))))
  :config
  ;; Determines the style used by `doom-modeline-buffer-file-name'.
  ;;
  ;; Given ~/Projects/FOSS/emacs/lisp/comint.el
  ;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
  ;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
  ;;   truncate-with-project => emacs/l/comint.el
  ;;   truncate-except-project => ~/P/F/emacs/l/comint.el
  ;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
  ;;   truncate-all => ~/P/F/e/l/comint.el
  ;;   relative-from-project => emacs/lisp/comint.el
  ;;   relative-to-project => lisp/comint.el
  ;;   file-name => comint.el
  ;;   buffer-name => comint.el<2> (uniquify buffer name)
  ;;
  (setq doom-modeline-buffer-file-name-style 'relative-from-project)

  ;; Whether show `all-the-icons' or not (when nil nothing will be
  ;; showed).
  (setq doom-modeline-icon nil)

  ;; How tall the mode-line should be (only respected in GUI Emacs).
  (setq doom-modeline-height 25)

  ;; How wide the mode-line bar should be (only respected in GUI
  ;; Emacs).
  (setq doom-modeline-bar-width 1)

  ;; Whether display minor modes or not. Non-nil to display in
  ;; mode-line.
  (setq doom-modeline-minor-modes nil)

  ;; If non-nil, the mode-line is displayed with the `variable-pitch'
  ;; face.
  (setq doom-modeline-enable-variable-pitch nil)


  (doom-modeline-mode 1))

(provide 'pkg-doom-modeline)

;;; pkg-doom-modeline.el ends here
