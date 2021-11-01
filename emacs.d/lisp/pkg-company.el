;;; pkg-company.el --- -*- lexical-binding: t; -*-

;;; Modular completion framework.

(require 'use-package)

(use-package company
  :straight t
  :hook (after-init . global-company-mode)
  :commands (company-complete-common
             company-manual-begin
             company-grab-line))

(provide 'pkg-company)

;;; pk-company.el ends here
