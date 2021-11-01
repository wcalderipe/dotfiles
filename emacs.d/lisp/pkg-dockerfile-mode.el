;;; pkg-dockerfile-mode.el --- -*- lexical-binding: t; -*-

(require 'use-package)

(use-package dockerfile-mode
  :straight t
  ;; Any file starting with "Dockerfile" should enable this mode.
  :mode (("^Dockerfile" . dockerfile-mode)))

(provide 'pkg-dockerfile-mode)

;;; pkg-dockerfile-mode.el ends here
