;; More convenient key definitions.
(use-package general
  :straight t

  :demand t

  :config
  (general-define-key
   ;; Kill the current buffer by default.
   "C-x k" #'kill-this-buffer))

;; Let's be honest here, there's nothing more productive than vi
;; key bindings in the right hands.
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

  :after
  (evil)

  :config
  (evil-collection-init))


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

  ;; Show fine differences for the current diff hunk only.
  (setq magit-diff-refine-hunk t)

  (general-define-key
   :states 'normal
   :keymaps 'override
   :prefix my/leader
   "gs" #'magit-status))


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


;; Various completion functions.
(use-package counsel
  :straight t

  :defer t

  :init
  (setq counsel-describe-function-function #'helpful-callable
	counsel-describe-variable-function #'helpful-variable)

  ;; Use custom configurations, but most importantly, pipe the filtering
  ;; from the "fdfind" output. See the `dotfiles/shell/vars' file for
  ;; more details.
  (setq counsel-fzf-cmd
	(concat (getenv "FZF_CTRL_T_COMMAND") " | " "fzf -f \"%s\""))

  (general-define-key
   [remap bookmark-jump] #'counsel-bookmark
   [remap describe-variable] #'counsel-describe-variable
   [remap describe-function] #'counsel-describe-function
   [remap find-file] #'counsel-find-file
   [remap org-set-tags-command] #'counsel-org-tag
   [remap execute-extended-command] #'counsel-M-x)

  (general-define-key
   :prefix my/leader
   :states 'normal
   :keymaps 'override
   "f" #'counsel-projectile-find-file))


;; Ivy - a generic completion frontend for Emacs.
(use-package ivy
  :straight t

  :defer t

  :hook (emacs-startup . ivy-mode)

  :init
  ;; Avoid using fuzzy searches everywhere. For example, counsel-rg
  ;; with fuzzy enabled brings a lot of useless results.
  ;; Remember you can switch modes in the ivy minibuffer with <C-o S-m>.
  (setq ivy-re-builders-alist '((counsel-M-x . ivy--regex-fuzzy)
				(t . ivy--regex-plus)))

  ;; Do not display the total number of candidates.
  (setq ivy-count-format "")

  ;; Only show the current directory.
  (setq ivy-extra-directories '("./"))

  ;; Do not close the minibuffer when there's no text left to delete.
  (setq ivy-on-del-error-function #'ignore)

  (general-define-key
   [remap switch-to-buffer] #'ivy-switch-buffer
   [remap list-buffers] #'ivy-switch-buffer
   [remap switch-to-buffer-other-window] #'ivy-switch-buffer-other-window))


;; Gives Ivy the ability to show recently used M-x commands.
(use-package amx
  :straight t

  :init

  (setq amx-save-file (concat my/cache-dir "amx")
	amx-history-length 10))


;; Gives an overview of the current regex search candidates.
(use-package swiper
  :straight t

  :defer t

  :init
  (setq swiper-action-recenter t)

  (general-define-key
   [remap switch-to-buffer] #'ivy-switch-buffer
   [remap list-buffers] #'ivy-switch-buffer
   [remap switch-to-buffer-other-window] #'ivy-switch-buffer-other-window
   [remap bookmark-jump] #'counsel-bookmark
   [remap describe-variable] #'counsel-describe-variable
   [remap describe-function] #'counsel-describe-function
   [remap find-file] #'counsel-find-file
   [remap execute-extended-command] #'counsel-M-x)

  (general-define-key
   :prefix my/leader
   :states 'normal
   :keymaps 'override
   "/" #'swiper))


;; Projectile is a project interaction library for Emacs. Its goal is
;; to provide a nice set of features operating on a project level
;; without introducing external dependencies (when feasible).
;; NOTE: It require https://github.com/sharkdp/fd
(use-package projectile
  :straight t

  ;; Defer because it'll be loaded by counsel-projectile.
  :defer t

  :init
  (setq projectile-cache-file (concat my/cache-dir "projectile.cache")
	projectile-enable-caching nil
	projectile-ignored-projects '("~/" "/tmp")
	projectile-known-projects-file (concat my/cache-dir "projectile-bookmarks.eld")
	;; Enable Projectile in every directory (even without the presence
	;; of project file). This works well with fd, given how much faster
	;; it is compared to find.
	projectile-require-project-root t
	projectile-completion-system 'ivy)

  (global-set-key [remap evil-jump-to-tag] #'projectile-find-tag)
  (global-set-key [remap find-tag]         #'projectile-find-tag)

  ;; It's recommended to use fd as a replacement for both git ls-files
  ;; and find.
  (setq projectile-generic-command "fdfind . --color=never --type f -0 -H -E .git"
	projectile-git-command projectile-generic-command)

  ;; Skip warnings about unsafe variables in .dir-locals.el
  (put 'projectile-project-type 'safe-local-variable #'symbolp)

  ;; Always open the top-level project directory after switching projects.
  (setq projectile-switch-project-action #'projectile-dired)

  :config
  (general-define-key
   :prefix my/leader
   :states 'normal
   :keymaps 'prog-mode-map
   "jA" #'projectile-find-implementation-or-test-other-window
   "ja" #'projectile-toggle-between-implementation-and-test))


;; Ivy UI for Projectile.
(use-package counsel-projectile
  :straight t

  :defer 0

  :config
  ;; Counsel-Projectile mode turns on Projectile mode, thus enabling all
  ;; projectile key bindings, and adds the counsel-projectile key bindings on
  ;; top of them.
  (counsel-projectile-mode +1)

  ;; Always open the top-level project directory after switching projects.
  (counsel-projectile-modify-action
   'counsel-projectile-switch-project-action
   '((default counsel-projectile-switch-project-action-dired))))

(provide 'my-packages-core)
