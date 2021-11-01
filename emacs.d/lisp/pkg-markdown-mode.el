;;; pkg-markdown-mode.el --- -*- lexical-binding: t; -*-

(require 'use-package)

(use-package markdown-mode
  :straight t
  :defer t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :hook (markdown-mode . flyspell-mode)
  :init
  ;; Set the command which will open and preview files.
  (setq markdown-command "markdown"))

(provide 'pkg-markdown-mode)

;;; pkg-markdown-mode.el ends here
