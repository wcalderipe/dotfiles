;; Don't use hard tabs.
(setq-default indent-tabs-mode nil)

;; Do not automatically save changes to recovery files.
(setq auto-save-default nil)

;; Do not create backup files.
(setq make-backup-files nil)

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

;; No need for ~ files when editing.
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup.
(setq inhibit-startup-message t)

;; Skip warnings about unsafe variables in .dir-locals.el
(put 'projectile-project-type 'safe-local-variable #'symbolp)

;; Do not change to the buffer working directory.
(add-hook 'find-file-hook
          (lambda ()
            (setq default-directory command-line-default-directory)))

(load-theme 'zenburn t)
(set-frame-parameter nil 'background-mode 'dark)
(set-terminal-parameter nil 'background-mode 'dark)

;; Turn off menu bar and tool bar at the top of each frame.
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Increase font size for better readability.
(set-face-attribute 'default nil :height 130)

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

;; Highlight current line
(global-hl-line-mode 1)

;; Buffer settings
(setq-default indicate-empty-lines t)
(setq-default show-trailing-whitespace t)

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

(provide 'editor.init)
;;; editor.init.el ends here
