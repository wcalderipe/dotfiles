;;; pkg-typescript-mode.el --- -*- lexical-binding: t; -*-

(require 'use-package)

(use-package typescript-mode
  :straight t
  :defer t
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(provide 'pkg-typescript-mode)

;;; pkg-typescript-mode.el ends here
