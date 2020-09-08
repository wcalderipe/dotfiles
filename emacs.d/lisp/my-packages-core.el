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
  :after (evil)
  :config (evil-collection-init))


(use-package org
  :straight t
  :commands (org-mode)
  :preface
  (setq org-directory my/org-dir
        ;; Stores notes in the Org directory.
        org-default-notes-file (concat my/org-dir "/notes.org")
        ;; Stores ID locations in the cache directory.
        org-id-locations-file (concat my/cache-dir "org-id-locations"))

  ;; Removes footnote HTML validation link.
  (setq org-html-validation-link nil)

  ;; Opens org files with all headlines visible.
  (setq org-startup-folded 'showall)

  ;; Records a timestamp when a todo item is DONE.
  (setq org-log-done 'time)

  ;; Place tags directly after headline text, with only one space in between.
  (setq org-tags-column 0)

  (setq org-todo-keywords
        '((sequence
           "TODO"
           "DONE")))

  :config
  ;; Disables auto indentation in BEGIN blocks. Let me handle it.
  (add-hook 'org-mode-hook (lambda () (electric-indent-mode -1)))

  ;; Languages which can be evaluated in Org buffers.
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t))))

;; Supplemental evil-mode key-bindings to org-mode.
(use-package evil-org
  :straight t
  :after (org evil)
  :commands (evil-org evil-org-agenda)
  :init (add-hook 'org-mode-hook 'evil-org-mode))


;; Effortless non-hierarchical note-taking with Org-mode.
(use-package org-roam
  :straight t
  :ensure t
  :hook (after-init . org-roam-mode)
  :init
  ;; Custom capture template with my `setup.org' adapted from
  ;; https://www.orgroam.com/manual/Template-Walkthrough.html#Template-Walkthrough
  (defun my/org-roam--capture-templates ()
    "Create the default org-roam capture template with a custom head."
    '(("d" "default" plain #'org-roam-capture--get-point
       "%?"
       :file-name "%<%Y%m%d%H%M%S>-${slug}"
       :unnarrowed t
       :head "#+setupfile: ~/dev/dotfiles/setup.org
#+title: ${title}\n")))

  (setq org-roam-directory my/org-dir
        org-roam-db-location (concat my/org-dir ".config/org-roam.db"))

  (setq org-roam-capture-templates (my/org-roam--capture-templates))

  :config
  (general-define-key
   :keymaps 'org-roam-mode-map
   "C-c n l" #'org-roam
   "C-c n f" #'org-roam-find-file
   "C-c n g" #'org-roam-graph-show)

  (general-define-key
   :keymaps 'org-mode-map
   "C-c n i" #'org-roam-insert
   "C-c n I" #'org-roam-insert-immediate))


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

  ;; Remove "^" prefixes of my searches.
  (setq ivy-initial-inputs-alist nil)

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

  ;; Enable bookmarks and recentf.
  (setq ivy-use-virtual-buffers t)

  (setq ivy-initial-inputs-alist nil)

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
  ;; Defer because it will be loaded by counsel-projectile.
  :defer t
  :init
  (defun my/copy-file-relative-name-to-clipboard ()
    "Copy current buffer relative file name to the clipboard."
    (interactive)
    (kill-new (file-relative-name buffer-file-name (projectile-project-root))))

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


;; Swap buffers easily (great combo with the hydra package).
(use-package buffer-move
  :straight t
  :defer t)


;; A simple-minded way of managing window configs.
;; IMPORTANT: if defered, eyebrowse won't work hydra.
(use-package eyebrowse
  :straight t
  :init
  ;; Use the scratch buffer when creating new tabs.
  (setq eyebrowse-new-workspace t)

  ;; Cycle through tabs.
  (setq eyebrowse-wrap-around t)

  :config
  (eyebrowse-mode t))


;; Fast search interface using Ripgrep.
(use-package deadgrep
  :straight t
  :commands (deadgrep)
  :config
  (with-eval-after-load 'evil
    ;; Update jump list before leaving the deadgrep buffer.
    (evil-add-command-properties #'deadgrep-visit-result :jump t)))


(use-package hydra
  :straight t
  :defer t
  :init
  (defun my/counsel-projectile-switch-project-action-dired (project)
    "Open dired when switching projects with counsel-projectile."
    (let ((projectile-switch-project-action
           (lambda ()
             (projectile-dired))))
      (counsel-projectile-switch-project-by-name project)))

  (defun my/counsel-projectile-switch-project-dotfiles ()
    "Open my dotfiles project straightaway."
    (interactive)
    (my/counsel-projectile-switch-project-action-dired "~/dev/dotfiles"))

  (defun my/dired-dotfiles-toggle ()
    "Show/hide dotfiles"
    (interactive)
    (when (equal major-mode 'dired-mode)
      ;; If currently showing
      (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p)
          (progn
            (set (make-local-variable 'dired-dotfiles-show-p) nil)
            (dired-mark-files-regexp "^\\\.")
            (dired-do-kill-lines))
        ;; Otherwise just revert to re-show
        (progn (revert-buffer)
               (set (make-local-variable 'dired-dotfiles-show-p) t)))))

  (defun my/async-shell-command-no-window (command)
    "Execute string COMMAND asynchronously without opening buffer."
    (interactive "sAsync shell command: ")
    (let* ((buffer-name "*Async Shell Command*")
           (output-buffer (get-buffer-create buffer-name))
           (process (let ((display-buffer-alist (list (list buffer-name #'display-buffer-no-window))))
                      (async-shell-command command output-buffer)
                      (get-buffer-process output-buffer)))
           (sentinel `(lambda (process signal)
                        (when (memq (process-status process) '(exit signal))
                          (shell-command-sentinel process signal)
                          ;; Here you could run arbitrary code when the
                          ;; command is successful.
                          ;; (when (zerop (process-exit-status process))
                          ;;   (message "%s" ,cmd))
                          ))))
      (when (process-live-p process)
        (set-process-sentinel process sentinel))))

  (defun my/async-shell-region-no-window (begin end)
    "Execute the REGION as a COMMAND asynchronously without opening buffer."
    (interactive "r")
    (my/async-shell-command-no-window
     (buffer-substring-no-properties begin end)))

  (defun my/projectile-run-async-shell-command-no-window-in-root ()
    "Invoke `my/async-shell-command-no-window' in the project's root."
    (interactive)
    (projectile-with-default-dir (projectile-ensure-project (projectile-project-root))
      (call-interactively 'my/async-shell-command-no-window)))

  (defun my/window-resize-right (arg)
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
      (shrink-window-horizontally arg)))

  (defun my/window-resize-left (arg)
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
      (enlarge-window-horizontally arg)))

  (defun my/window-resize-up (arg)
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
      (enlarge-window arg)
      (shrink-window arg)))

  (defun my/window-resize-down (arg)
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
      (shrink-window arg)
      (enlarge-window arg)))

  :config
  ;; For more hydra examples, have a look at:
  ;; https://github.com/abo-abo/hydra/blob/master/hydra-examples.el

  (defhydra hydra-projectile (:hint nil :foreign-keys nil :exit t :idle 0.5)
    "
PROJECT: %(projectile-project-root)

 ^Search^                ^Buffers^                    ^Cache^                 ^Command
^^^^^^^----------------------------------------------------------------------------------------------
 _s_: rg (mini-buffer)   _a_: alternate               _c_: cache clear        _r_ run async
 _S_: rg (deadgrep)      _A_: alternate (other win)   _i_: include project    _R_ run async no window
 ^ ^                     _b_: switch to buffer        _x_: remove project     _C_ compile
 ^ ^                     _d_: dired                   _X_: cleanup projects   _T_ test
 ^ ^                     _K_: kill all buffers        _z_: cache current

"
    ("A" projectile-find-implementation-or-test-other-window)
    ("C" projectile-compile-project)
    ("K" projectile-kill-buffers)
    ("R" my/projectile-run-async-shell-command-no-window-in-root)
    ("S" deadgrep)
    ("T" projectile-test-project)
    ("X" projectile-cleanup-known-projects)
    ("a" projectile-toggle-between-implementation-and-test)
    ("b" counsel-projectile-switch-to-buffer)
    ("c" projectile-invalidate-cache)
    ("d" projectile-dired)
    ("i" projectile-add-known-project)
    ("r" projectile-run-async-shell-command-in-root)
    ("s" counsel-projectile-rg)
    ("x" projectile-remove-known-project)
    ("z" projectile-cache-current-file)

    ("p" counsel-projectile-switch-project "switch project")
    ("." my/counsel-projectile-switch-project-dotfiles "switch dotfiles")
    ("q" nil "quit"))

  (defhydra hydra-dired (:hint nil :foreign-keys run :exit nil)
    "
_v_: view         _m_: mark           _l_: redisplay       _i_: insert subdir   wdired
_V_: view other   _u_: unmark         _g_: refresh         _$_: hide subdir     C-x C-q: edit
_o_: open other   _U_: unmark all     _=_: diff            _w_: kill subdir     C-c C-c: commit
_M_: chmod        _t_: toggle marks   _s_: sort            _X_: shell command   C-c ESC: abort
_G_: chgrp        _S_: symlink        _H_: toggle hidden
_O_: chown        _Z_: zip/unzip
^ ^
"
    ("$" diredp-hide-subdir-nomove)
    ("=" diredp-ediff)
    ("G" dired-do-chgrp)
    ("H" my/dired-dotfiles-toggle)
    ("M" dired-do-chmod)
    ("O" dired-do-chown)
    ("S" dired-do-symlink)
    ("T" dired-hide-details-mode)
    ("U" dired-unmark-all-marks)
    ("V" dired-display-file)
    ("X" dired-do-shell-command)
    ("Z" dired-do-compress)
    ("e" dired-ediff-files)
    ("g" revert-buffer)
    ("i" dired-maybe-insert-subdir)
    ("l" dired-do-redisplay)
    ("m" dired-mark)
    ("o" dired-find-file-other-window)
    ("s" dired-sort-toggle-or-edit)
    ("t" dired-toggle-marks)
    ("u" dired-unmark)
    ("v" dired-view-file)
    ("w" dired-kill-subdir)

    ("C" dired-do-copy          "copy")
    ("D" dired-do-delete        "remove")
    ("+" dired-create-directory "mkdir")
    ("R" dired-do-rename        "rename")
    ("q" nil                    "quit"))

  (defhydra hydra-window (:hint nil :exit nil)
    ("=" balance-windows "balance windows")
    ("h" buf-move-left "swap left")
    ("j" buf-move-down "swap down")
    ("k" buf-move-up "swap up")
    ("l" buf-move-right "swap right")
    ("u" winner-undo "undo")
    ("r" winner-redo "redo")
    ("q" nil "quit"))

  (defhydra hydra-tab (:hint nil :exit nil)
    ("+" eyebrowse-create-window-config "create")
    ("-" eyebrowse-close-window-config "remove")
    ("l" eyebrowse-next-window-config "next")
    ("h" eyebrowse-prev-window-config "previous")
    ("q" nil "quit"))

  (general-define-key
   :prefix "C-c"
   "w" #'hydra-window/body
   "p" #'hydra-projectile/body
   "t" #'hydra-tab/body)

  (general-define-key
   :keymaps 'dired-mode-map
   "C-c d" #'hydra-dired/body))

(provide 'my-packages-core)

;;; my-packages-core ends here.
