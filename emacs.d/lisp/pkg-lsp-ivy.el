;;; pkg-lsp-ivy.el --- -*- lexical-binding: t; -*-

(require 'use-package)

(use-package lsp-ivy
  :straight t
  :commands lsp-ivy-workspace-symbol)

(provide 'pkg-lsp-ivy)

;;; pkg-lsp-ivy.el ends here
