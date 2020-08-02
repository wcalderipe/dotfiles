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
  (add-to-list 'all-the-icons-dir-icon-alist '("emacs\\.d" all-the-icons-fileicon "emacs")))


;; Ivy/counsel integration for `all-the-icons'.
(use-package all-the-icons-ivy
  :when my/gui?
  :straight t
  :after (ivy counsel-projectile)
  :config
  ;; Adds icons to counsel-projectile-find-file as well.
  (setq all-the-icons-ivy-file-commands '(counsel-projectile-find-file))

  (all-the-icons-ivy-setup))


;; Displays icons for all buffers in Ivy.
(use-package all-the-icons-ivy-rich
  :when my/gui?
  :straight t
  :init (all-the-icons-ivy-rich-mode 1))


;; Adds dired support to all-the-icons.
(use-package all-the-icons-dired
  :when my/gui?
  :straight t
  :defer t
  :hook (dired-mode . all-the-icons-dired-mode))

;; Easily adjust the font size in all Emacs frames.
(use-package default-text-scale
  :when my/gui?
  :straight t
  :defer t
  :init
  (general-define-key
   "M-=" #'default-text-scale-increase
   "M--" #'default-text-scale-decrease
   "M-0" #'default-text-scale-reset))

(provide 'my-packages-gui)
