;;; pkg-counsel-projectile.el --- -*- lexical-binding: t; -*-

;;; Ivy UI for Projectile.

(require 'use-package)

(use-package counsel-projectile
  :straight t
  :defer 0
  :config
  ;; Counsel-Projectile mode turns on Projectile mode, thus enabling all
  ;; projectile key bindings, and adds the counsel-projectile key bindings on
  ;; top of them.
  (counsel-projectile-mode +1)

  ;; Always open the top-level project directory after switching projects.
  (counsel-projectile-modify-action
   'counsel-projectile-switch-project-action
   '((default counsel-projectile-switch-project-action-dired))))

(provide 'pkg-counsel-projectile)

;;; pkg-counsel-projectile.el ends here
