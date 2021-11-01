;;; pkg-all-the-icons.el --- -*- lexical-binding: t; -*-

(require 'use-package)

(use-package all-the-icons
  :when my/gui?
  :straight t
  :commands (all-the-icons-octicon
             all-the-icons-faicon
             all-the-icons-fileicon
             all-the-icons-wicon
             all-the-icons-material
             all-the-icons-alltheicon)

  :config
  ;; IMPORTANT: changing the variables below may require restarting
  ;; Emacs.
  ;; IMPORTANT: if placeholders are being displayed instead of icons
  ;; see https://github.com/domtronn/all-the-icons.el#troubleshooting

  (setq all-the-icons-ivy-rich-icon-size 1.0)

  ;; Icons by file name.
  (add-to-list 'all-the-icons-icon-alist '("\\.conf$" all-the-icons-octicon "settings" :height 1.0 :v-adjust 0.0 :face all-the-icons-dyellow))
  (add-to-list 'all-the-icons-icon-alist '("\\.service$" all-the-icons-octicon "settings" :height 1.0 :v-adjust 0.0 :face all-the-icons-dyellow))
  (add-to-list 'all-the-icons-icon-alist '("^config$" all-the-icons-octicon "settings" :height 1.0 :v-adjust 0.0 :face all-the-icons-dyellow))

  ;; Icons by directory name.
  (add-to-list 'all-the-icons-dir-icon-alist '("emacs" all-the-icons-fileicon "emacs"))
  (add-to-list 'all-the-icons-dir-icon-alist '("emacs\\.d" all-the-icons-fileicon "emacs"))
  (add-to-list 'all-the-icons-dir-icon-alist '("spec" all-the-icons-fileicon "test-dir")))

(provide 'pkg-all-the-icons)

;;; pkg-all-the-icons.el ends here
