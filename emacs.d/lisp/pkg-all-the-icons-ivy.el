;;; pkg-all-the-icons-ivy.el --- -*- lexical-binding: t; -*-

;;; Ivy/counsel integration for `all-the-icons'.

(require 'use-package)

(use-package all-the-icons-ivy
  :when my/gui?
  :straight t
  :after (ivy counsel-projectile)
  :config
  ;; Adds icons to counsel-projectile-find-file as well.
  (setq all-the-icons-ivy-file-commands '(counsel-projectile-find-file))

  (all-the-icons-ivy-setup))

(provide 'pkg-all-the-icons-ivy)

;;; pkg-all-the-icons-ivy.el ends here
