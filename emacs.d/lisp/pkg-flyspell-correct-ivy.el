;;; pkg-flyspell-correct-ivy.el --- -*- lexical-binding: t; -*-

;;; Distraction-free words correction with flyspell via Ivy.

(require 'use-package)

(use-package flyspell-correct-ivy
  :straight t
  :after (ivy)
  :defer t
  :hook (flyspell-mode . pkg-flyspell-correct-ivy/enable-flyspell-correct-ivy)
  :preface
  (defun pkg-flyspell-correct-ivy/enable-flyspell-correct-ivy ()
    (interactive)
    (require 'flyspell-correct-ivy))

  :config
  (general-define-key
   :keymaps 'flyspell-mode-map
   "C-;" 'flyspell-correct-wrapper))

(provide 'pkg-flyspell-correct-ivy)

;;; pkg-flyspell-correct-ivy.el ends here
