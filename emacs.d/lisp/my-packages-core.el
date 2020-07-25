;; More convenient key definitions.
(use-package general
  :straight t

  :demand t

  :config
  (general-define-key
   ;; Kill the current buffer by default.
   "C-x k" #'kill-this-buffer))

(use-package org
  :straight t

  :commands (org-mode)

  :config
  ;; Record a timestamp when a todo item is DONE.
  (setq org-log-done 'time))

;; Supplemental evil-mode key-bindings to org-mode.
(use-package evil-org
  :straight t

  :after
  (org evil)

  :commands
  (evil-org evil-org-agenda)

  :init
  (add-hook 'org-mode-hook 'evil-org-mode))

;; Let's be honest here, there's nothing more productive than vi
;; key-bindings in the right hands.
(use-package evil
  :straight t

  :preface
  ;; Do not load evil keybindings, because we'll use
  ;; from the evil-collection package.
  (setq evil-want-keybinding nil)

  :init
  (defun my/evil-vim-split ()
    "Splits the current window horizontally and switch to the new window."
    (interactive)
    (evil-window-split)
    (evil-window-down 1))

  (defun my/evil-vim-vsplit ()
    "Splits the current window vertically and switch to the new window."
    (interactive)
    (evil-window-vsplit)
    (evil-window-right 1))

  (defun my/evil-enable-visual-line-navigation ()
    "Simulate evil navigation in `visual-line-mode'."
    (define-key evil-motion-state-map "0" #'evil-beginning-of-visual-line)
    (define-key evil-motion-state-map "$" #'evil-end-of-visual-line)
    (define-key evil-motion-state-map "j" #'evil-next-visual-line)
    (define-key evil-motion-state-map "k" #'evil-previous-visual-line))

  ;; Allows jumping back and forth between special buffers too.
  (setq evil--jumps-buffer-targets "\\*")

  ;; Always start in the normal mode. This is required, for example, to not enter
  ;; the git commit mode in insert mode. More often than not I have to navigate
  ;; across the diff before knowing what to write in the commit message.
  (add-hook 'with-editor-mode-hook #'evil-normal-state)

  ;; With visual-line-mode enabled it's better to navigate by visual line.
  (add-hook 'visual-line-mode-hook #'my/evil-enable-visual-line-navigation)

  ;; Always center current line while searching.
  (defadvice evil-search-next
      (after advice-for-evil-search-next activate)
    (evil-scroll-line-to-center (line-number-at-pos)))
  (defadvice evil-search-previous
      (after advice-for-evil-search-previous activate)
    (evil-scroll-line-to-center (line-number-at-pos)))

  :config
  ;; Split like Vim, i.e. moves to the newly created window.
  (evil-ex-define-cmd "split"  #'my/evil-vim-split)
  (evil-ex-define-cmd "vsplit" #'my/evil-vim-vsplit)

  (general-define-key
   :keymaps 'evil-motion-state-map
   [remap evil-beginning-of-line] #'my/smart-beginning-of-line)

  (general-define-key
   :keymaps 'evil-normal-state-map
   "C-]" #'evil-goto-definition
   ;; Remove bindings conflicting with default Emacs behavior.
   "M-." nil
   "C-p" nil
   "C-n" nil)

  (evil-mode 1))

;; Add evil bindings beyond the default like calendar and help-mode.
(use-package evil-collection
  :straight t

  :defer t

  :hook
  (evil-mode . evil-collection-init))

;; Better file navigation with dired.
(use-package dired
  :init
  (defun my/dired-hidden-toggle ()
    "Show/hide dot-files."
    (interactive)
    (when (equal major-mode 'dired-mode)
      (if (or (not (boundp 'my/dired-hidden-show-p))
              my/dired-hidden-show-p)
          ;; If currently showing.
          (progn
            (setq-local my/dired-hidden-show-p nil)
            (dired-mark-files-regexp "^\\\.")
            (dired-do-kill-lines))
        ;; Otherwise just revert to re-show.
        (progn (revert-buffer)
               (setq-local my/dired-hidden-show-p t)))))

  (setq dired-listing-switches
        (string-join '("-l"
                       "--almost-all"
                       "--classify"
                       "--dired"
                       "--group-directories-first"
                       "--human-readable")
                     " "))

  ;; Always copy/delete recursively.
  (setq dired-recursive-copies  'always
        dired-recursive-deletes 'top)

  ;; Where to store image caches.
  (setq image-dired-dir (concat my/cache-dir "image-dired/")
        image-dired-db-file (concat image-dired-dir "db.el")
        image-dired-gallery-dir (concat image-dired-dir "gallery/")
        image-dired-temp-image-file (concat image-dired-dir "temp-image")
        image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image"))

  (setq dired-auto-revert-buffer t
	;; Suggest a target for moving/copying intelligently
        dired-dwim-target t 
        dired-hide-details-hide-symlink-targets nil)

  ;; Disable the prompt about whether I want to kill the Dired buffer for a
  ;; deleted directory.
  (setq dired-clean-confirm-killing-deleted-buffers nil)

  ;; Screens are larger nowadays, we can afford slightly larger thumbnails
  (setq image-dired-thumb-size 150)

  (add-hook 'dired-mode-hook #'dired-hide-details-mode)

  :config
  (general-define-key
   [remap dired] #'counsel-dired
   "C-x C-j" #'dired-jump
   "C-x 4 j" #'dired-jump-other-window))

;; Magit is an interface to Git.
(use-package magit
  :straight t

  :commands (magit)

  :init
  ;; If a buffer's major-mode derives from magit-diff-mode or magit-process-mode,
  ;; display it in another window. Display all other buffers in the selected
  ;; window.
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

  ;; File used to save history of transients and their infixes.
  (setq transient-history-file (concat my/cache-dir "transient/history.el"))

  ;; Enable spell check automatically.
  (add-hook 'git-commit-mode-hook #'flyspell-mode)

  ;; Improve diff performance.
  ;; (setq magit-diff-paint-whitespace nil
  ;;       magit-diff-adjust-tab-width nil
  ;;       magit-diff-hide-trailing-cr-characters nil)

  ;; Show fine differences for the current diff hunk only.
  (setq magit-diff-refine-hunk t))

;; Evil keybindings for Magit.
(use-package evil-magit
  :straight t

  :defer t

  :hook (magit-mode . evil-magit-init)

  :init
  (setq evil-magit-state 'normal))

;; Allows Emacs to copy and to paste from the system clipboard.
(use-package xclip
  :straight t

  :defer t

  :init (xclip-mode t))

(provide 'my-packages-core)
