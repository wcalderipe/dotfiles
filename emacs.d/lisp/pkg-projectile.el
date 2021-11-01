;;; pkg-projectile.el --- -*- lexical-binding: t; -*-

;;; Projectile is a project interaction library for Emacs. Its goal is to
;;; provide a nice set of features operating on a project level without
;;; introducing external dependencies (when feasible).
;;;
;;; NOTE: It require https://github.com/sharkdp/fd

(require 'use-package)

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
  (setq projectile-generic-command (concat (if (my/macos?) "fd" "fdfind") " . --color=never --type f -0 -H -E .git")
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
   "j A" #'projectile-find-implementation-or-test-other-window
   "j a" #'projectile-toggle-between-implementation-and-test))

(provide 'pkg-projectile)

;;; pkg-projectile.el ends here
