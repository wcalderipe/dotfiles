;; Disable indentation with tabs.
(setq-default indent-tabs-mode nil)

;; Do not automatically save changes.
(setq auto-save-default nil)

(setq make-backup-files t)
(setq vc-make-backup-files t) ; Backup version controlled files
(setq backup-by-copying t)    ; Don't clobber symlinks
(setq delete-old-versions t)  ; Don't ask about deleting old versions
(setq kept-new-versions 10)   ; Keep 10 latest versions
(setq kept-old-versions 2)    ; Keep 2 oldest versions
(setq version-control t)      ; Number backups

;;; Avoid littering the user's filesystem with backups.
(setq backup-directory-alist
  `((".*" . ,temporary-file-directory)))

;; Do not use lockfiles to avoid editing collisions.
(setq create-lockfiles nil)

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; Keep track of saved places in ~/.emacs.d/places
(save-place-mode 1)

;; Enable folding by indentation, just like Vim when using Evil.
(add-hook 'prog-mode-hook #'hs-minor-mode)

;; Change all cursor movement/edit commands to stop in-between the
;; "camelCase" words.
(add-hook 'prog-mode-hook #'subword-mode)

;; Automatic (electric) indentation introduced since Emacs 24.1.
;; Electric indendation should be enabled carefully, as it breaks
;; some modes, including Python and Org.
(setq electric-indent-mode t)

;; Avoid problems with crontabs
(setq require-final-newline t)

;; Changes all yes/no questions to y/n type.
(fset 'yes-or-no-p 'y-or-n-p)

;; Go straight to scratch buffer on startup.
(setq inhibit-startup-message t)

;; Do not add useless text to the scratch buffer.
(setq initial-scratch-message "")

;; Do not display a startup message. Here the function is redefined
;; because the variable doesn't work as expected.
(setq inhibit-startup-echo-area-message t)
(defun display-startup-echo-area-message ())

;; Turn off menu bar and tool bar at the top of each frame.
(menu-bar-mode -1)
(when (display-graphic-p)
  (tool-bar-mode -1))

;; Increase font size for better readability.
(set-face-attribute 'default nil
                    :family "Fira Code"
                    :height 150)

;; Disable bold (font weight) globally.
(mapc
  (lambda (face)
    (set-face-attribute face nil :weight 'normal :underline nil))
  (face-list))

;; Don't show native OS scroll bars for buffers because they're redundant.
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; No cursor blinking, it's distracting.
(blink-cursor-mode 0)

;; Full path in title bar.
;; %b -- print buffer name.
;; %f -- print visited file name.
(setq-default frame-title-format "%b (%f)")

;; No bell.
(setq ring-bell-function 'ignore)

;; By default, there’s a small delay before showing a matching parenthesis. It
;; can be deactivated with the following (which you have to do before
;; activating show-paren-mode).
(setq show-paren-delay 0)

;; Highlights matching parenthesis.
(show-paren-mode 1)

;; Disable highlight line mode globally and enable it for specific
;; modes that are mostly "line oriented", like Dired.
(global-hl-line-mode 0)
(add-hook 'dired-mode-hook 'hl-line-mode)
(add-hook 'neotree-mode-hook 'hl-line-mode)

;; Increase the line-spacing between buffer’s lines (default is 0).
;; On text terminals the line spacing cannot be altered.
(setq-default line-spacing 2)

;; Buffer settings
(setq-default indicate-empty-lines t)
(setq-default show-trailing-whitespace nil)

(when (window-system)
  (set-frame-size (selected-frame) 135 40))

;; Smooth scrolling
(setq scroll-margin 2
      scroll-conservatively 9999
      scroll-step 1)

;; No word-wrap
(set-default 'truncate-lines t)

;; Remove trailing whitespace before saving.
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Start Emacs GUI maximized. No effect on terminal Emacs.
(add-hook 'after-init-hook #'toggle-frame-maximized)

;; Enable winner mode so that I can undo/redo window changes.
(winner-mode 1)

;; Split ediff windows side-by-side (similar to vimdiff).
(setq ediff-split-window-function 'split-window-horizontally)

;; I set it to fundamental-mode so as to avoid unnecessary prog-mode
;; or emacs-lisp-mode hooks being called on startup.
(setq initial-major-mode 'fundamental-mode)

;; Follow symlinks without asking. The default configuration is safer,
;; but I use symlinks extensively.
(setq vc-follow-symlinks t)

;; Setup custom settings file.
(setq custom-file (locate-user-emacs-file "custom.el"))
