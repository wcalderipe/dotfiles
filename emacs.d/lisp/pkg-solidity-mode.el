;;; pkg-solidity-mode.el --- -*- lexical-binding: t; -*-

(require 'use-package)

(use-package solidity-mode
  :straight t
  :defer t
  :mode "\\.sol\\'")

(provide 'pkg-solidity-mode)

;;; pkg-solidity-mode.el ends here
