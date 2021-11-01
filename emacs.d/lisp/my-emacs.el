;;; my-editor.el --- -*- lexical-binding: t; -*-

;;; Configuration changing Emacs' default settings.

;; Remove top menubar.
(menu-bar-mode -1)

;; Remove top tool bar (only respected in GUI Emacs).
(tool-bar-mode -1)

;; Remove scroll bar (only respected in GUI Emacs).
(toggle-scroll-bar -1)

;; Never save backup files
(setq make-backup-files nil)

;; UTF-8 as the default coding system.
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8
      selection-coding-system 'utf-8)

;; Smooth scrolling
(setq scroll-margin 2
      scroll-conservatively 9999
      scroll-step 1)

;; Do not use lockfiles to avoid editing collisions.
(setq create-lockfiles nil)

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; Keep track of saved places in ~/.emacs.d/places
(save-place-mode 1)

;; Do not automatically save changes.
(setq auto-save-default nil
      auto-save-list-file-name (concat my/cache-dir "autosave"))

;; Enable folding by indentation, just like Vim when using Evil.
(add-hook 'prog-mode-hook #'hs-minor-mode)

;; No cursor blinking.
(blink-cursor-mode 0)

;; No word-wrap.
(set-default 'truncate-lines t)

;; Enable winner mode so that I can undo/redo window changes.
(winner-mode 1)

;; Don't stretch the cursor to fit wide characters, it is disorienting,
;; especially for tabs.
(setq x-stretch-cursor nil)

;; Full path in title bar.
;; %b -- print buffer name.
;; %f -- print visited file name.
(setq-default frame-title-format "%b (%f)")

;; No bell.
(setq ring-bell-function 'ignore)

;; Changes all yes/no questions to y/n type.
(fset 'yes-or-no-p 'y-or-n-p)

;; Highlights matching parenthesis.
(add-hook 'prog-mode-hook #'show-paren-mode)

;; Don't highlight trailing whitespaces
(setq-default show-trailing-whitespace nil)

;; Remove trailing whitespace before saving.
(add-hook 'before-save-hook #'whitespace-cleanup)

;; Enable mouse in terminal Emacs.
(add-hook 'tty-setup-hook #'xterm-mouse-mode)

;; Remove hscroll-margin in shells, otherwise it causes jumpiness.
(dolist (mode '(eshell-mode-hook term-mode-hook vterm-mode-hook))
  (add-hook mode #'my/remove-horizontal-scroll-margin-in-shells))

;; Always avoid GUI.
(setq use-dialog-box nil)

;; Split ediff windows side-by-side (similar to vimdiff).
(setq ediff-split-window-function #'split-window-horizontally)

;; Follow symlinks without asking.
(setq vc-follow-symlinks t
      find-file-visit-truename t)

;; Disables VC altogether because I'm only using git and `magit'.
(setq vc-handled-backends nil)

;; Save custom settings in the cache directory.
(setq custom-file (concat my/cache-dir "custom.el"))

;; Indentation.
(setq-default tab-width 4
              tab-always-indent t
              indent-tabs-mode nil
              fill-column 80)

;; Set default font size (`:height' is 1/10pt).
;; This configuration is taking into account a 27" screen with a resolution of
;; 2560x1440 (16:9).
(set-face-attribute 'default nil :height 110)

;; Set fringe to 8 pixels wide.
(when (my/macos?)
  (fringe-mode '(8 . 8)))

;; Change the default load-theme with a custom function that removes set faces
;; before loading the new theme.
(advice-add 'load-theme :around #'my/load-theme-advice)

(provide 'my-emacs)

;;; my-emacs.el ends here
