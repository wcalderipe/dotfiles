;;; pkg-eyebrowse.el --- -*- lexical-binding: t; -*-

;;; A simple-minded way of managing window configs.
;;;
;;; IMPORTANT: if defered, eyebrowse won't work in hydra.

(require 'use-package)

(use-package eyebrowse
  :straight t
  :init
  ;; Use the scratch buffer when creating new tabs.
  (setq eyebrowse-new-workspace t)

  ;; Cycle through tabs.
  (setq eyebrowse-wrap-around t)

  :config
  (eyebrowse-mode t))

(provide 'pkg-eyebrowse)

;;; pkg-eyebrowse.el ends here
