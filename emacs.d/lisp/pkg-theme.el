;;; pkg-theme.el --- -*- lexical-binding: t; -*-

(require 'use-package)

(use-package pkg-theme
  :no-require t
  :config
  (load-theme 'doom-one t nil))

(provide 'pkg-theme)

;;; pkg-theme.el ends here
