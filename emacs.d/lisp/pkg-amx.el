;;; pkg-amx.el --- -*- lexical-binding: t; -*-

;;; Gives Ivy the ability to show recently used M-x commands.

(require 'use-package)

(use-package amx
  :straight t
  :init
  (setq amx-save-file (concat my/cache-dir "amx")
        amx-history-length 10))

(provide 'pkg-amx)

;;; pkg-amx.el ends here
