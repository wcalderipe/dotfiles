;;; pkg-dired.el --- -*- lexical-binding: t; -*-

;;; Better file navigation with dired.

(require 'use-package)

(use-package dired
  :config
  (defun pkg-dired/dired-hidden-toggle ()
    "Show/hide dot-files."
    (interactive)
    (when (equal major-mode 'dired-mode)
      (if (or (not (boundp 'pkg-dired/dired-hidden-show-p))
              pkg-dired/dired-hidden-show-p)
          ;; If currently showing.
          (progn
            (setq-local pkg-dired/dired-hidden-show-p nil)
            (dired-mark-files-regexp "^\\\.")
            (dired-do-kill-lines))
        ;; Otherwise just revert to re-show.
        (progn (revert-buffer)
               (setq-local pkg-dired/dired-hidden-show-p t)))))

  (setq dired-listing-switches
        (string-join '("-l"
                       "--almost-all"
                       "--classify"
                       "--dired"
                       "--group-directories-first"
                       "--human-readable")
                     " "))

  ;; Always copy/delete recursively.
  (setq dired-recursive-copies  'always
        dired-recursive-deletes 'top)

  ;; Where to store image caches.
  (setq image-dired-dir (concat my/cache-dir "image-dired/")
        image-dired-db-file (concat image-dired-dir "db.el")
        image-dired-gallery-dir (concat image-dired-dir "gallery/")
        image-dired-temp-image-file (concat image-dired-dir "temp-image")
        image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image"))

  (setq dired-auto-revert-buffer t
        ;; Suggest a target for moving/copying intelligently
        dired-dwim-target t
        dired-hide-details-hide-symlink-targets nil)

  ;; Disable the prompt about whether I want to kill the Dired buffer for a
  ;; deleted directory.
  (setq dired-clean-confirm-killing-deleted-buffers nil)

  ;; Screens are larger nowadays, we can afford slightly larger thumbnails
  (setq image-dired-thumb-size 150)

  (when (my/macos?)
    ;; Fix dired directory listing issue on MacOS by using GNU ls.
    ;; See https://emacs.stackexchange.com/q/53904
    (setq insert-directory-program "/opt/homebrew/opt/coreutils/bin/gls"))

  (add-hook 'dired-mode-hook #'dired-hide-details-mode)

  :config
  (general-define-key
   [remap dired] #'counsel-dired
   "C-x C-j" #'dired-jump
   "C-x 4 j" #'dired-jump-other-window))

(provide 'pkg-dired)

;;; pkg-dired.el ends here
