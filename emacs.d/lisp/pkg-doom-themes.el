;;; pkg-doom-themes.el --- -*- lexical-binding: t; -*-

(require 'use-package)

(use-package doom-themes
  :straight t
  :ensure t
  :config
  (doom-themes-org-config))

(provide 'pkg-doom-themes)

;;; pkg-doom-themes.el ends here
