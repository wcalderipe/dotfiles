;;; pkg-deadgrep.el --- -*- lexical-binding: t; -*-

;;; Fast search interface using Ripgrep.

(require 'use-package)

(use-package deadgrep
  :straight t
  :commands (deadgrep)
  :config
  (with-eval-after-load 'evil
    ;; Update jump list before leaving the deadgrep buffer.
    (evil-add-command-properties #'deadgrep-visit-result :jump t)))

(provide 'pkg-deadgrep)

;;; pkg-deadgrep.el ends here
