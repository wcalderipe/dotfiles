;;; pkg-flyspell.el --- -*- lexical-binding: t; -*-

;;; On-the-fly spell checking.

(require 'use-package)

(use-package flyspell
  :defer t
  :init
  (when (my/macos?)
    (setq ispell-program-name "aspell")))

(provide 'pkg-flyspell)

;;; pkg-flyspell.el ends here
