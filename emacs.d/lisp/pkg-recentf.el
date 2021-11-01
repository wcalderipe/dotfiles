;;; pkg-recentf.el --- -*- lexical-binding: t; -*-

;;; A minor mode that builds a list of recently opened files.

(require 'use-package)

(use-package recentf
  :no-require t
  :hook (kill-emacs . recentf-cleanup)
  :init
  (setq recentf-save-file (concat my/cache-dir "recentf")
        recentf-auto-cleanup 'never
        recentf-max-menu-items 0
        recentf-max-saved-items 200)

  :config
  ;; This hook should be in the config section because otherwise I get a
  ;; "function definition is void" error for the `recentf-add-file' function.
  (defun pkg-recentf/recentf-add-dired-directory ()
    "Add dired directory to recentf file list."
    (recentf-add-file default-directory))

  (add-hook 'dired-mode-hook #'pkg-recentf/recentf-add-dired-directory)

  (recentf-mode +1))

(provide 'pkg-recentf)

;;; pkg-recentf.el ends here
