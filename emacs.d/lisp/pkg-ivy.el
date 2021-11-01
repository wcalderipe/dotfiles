;;; pkg-ivy.el --- -*- lexical-binding: t; -*-

;;; Generic completion frontend for Emacs.

(require 'use-package)

(use-package ivy
  :straight t
  :defer t
  :hook (emacs-startup . ivy-mode)
  :init
  ;; Avoid using fuzzy searches everywhere. For example, counsel-rg
  ;; with fuzzy enabled brings a lot of useless results.
  ;; Remember you can switch modes in the ivy minibuffer with <C-o S-m>.
  (setq ivy-re-builders-alist '((counsel-M-x . ivy--regex-fuzzy)
                                (t . ivy--regex-plus)))

  ;; Do not display the total number of candidates.
  (setq ivy-count-format "")

  ;; Only show the current directory.
  (setq ivy-extra-directories '("./"))

  ;; Do not close the minibuffer when there's no text left to delete.
  (setq ivy-on-del-error-function #'ignore)

  ;; Enable bookmarks and recentf.
  (setq ivy-use-virtual-buffers t)

  (setq ivy-initial-inputs-alist nil)

  (general-define-key
   [remap switch-to-buffer] #'ivy-switch-buffer
   [remap list-buffers] #'ivy-switch-buffer
   [remap switch-to-buffer-other-window] #'ivy-switch-buffer-other-window))

(provide 'pkg-ivy)

;;; pkg-ivy.el ends here
