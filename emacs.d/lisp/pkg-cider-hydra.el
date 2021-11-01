;;; pkg-cider-hydra.el --- -*- lexical-binding: t; -*-

;;; Defines several hydras for CIDER.

(require 'use-package)

(use-package cider-hydra
  :straight t
  :defer t
  :after (cider)
  :hook ((clojure-mode clojurescript-mode) . cider-hydra-mode))

(provide 'pkg-cider-hydra)

;;; pkg-cider-hydra.el ends here
