;;; pkg-web-mode.el --- -*- lexical-binding: t; -*-

(require 'use-package)

(use-package web-mode
  :straight t
  :defer t
  :mode "\\.\\(?:\\(?:h\\(?:bs\\|tml\\)\\|liquid\\|tmpl\\)\\)\\'"
  :init
  (setq web-mode-markup-indent-offset 2)

  :config
  (general-define-key
   :keymaps 'evil-normal-state-map
   [remap evil-toggle-fold] #'web-mode-fold-or-unfold))

(provide 'pkg-web-mode)

;;; pkg-web-mode.el ends here
