;;; pkg-magit.el --- -*- lexical-binding: t; -*-

;;; Git user interface for Emacs.

(require 'use-package)

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
   :prefix my/leader
   :states 'normal
   :keymaps 'override
   "g s" #'magit-status))

(provide 'pkg-magit)

;;; pkg-magit.el ends here
