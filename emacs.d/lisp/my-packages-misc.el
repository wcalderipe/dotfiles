;; Displays available keybindings (discoverability).
(use-package which-key
  :straight t

  :config
  (which-key-setup-minibuffer)
  (which-key-mode))


;; More friendly interface for ivy (discoverability).
(use-package ivy-rich
  :straight t
  :defer t

  :config
  ;; These configurations were adapted from the README:
  ;; https://github.com/Yevgnen/ivy-rich
  (setq ivy-rich-display-transformers-list
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-candidate
             (:width 30))
            ((lambda (candidate)
               (file-name-directory (ivy-rich-switch-buffer-path candidate)))
             (:width (lambda (x)
                       (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate (lambda (candidate)
                        (get-buffer candidate)))

          counsel-M-x
          (:columns
           ((counsel-M-x-transformer
             (:width 40))
            (ivy-rich-counsel-function-docstring
             (:face font-lock-doc-face))))

          counsel-describe-function
          (:columns
           ((counsel-describe-function-transformer
             (:width 40))
            (ivy-rich-counsel-function-docstring
             (:face font-lock-doc-face))))

          counsel-describe-variable
          (:columns
           ((counsel-describe-variable-transformer
             (:width 40))
            (ivy-rich-counsel-variable-docstring
             (:face font-lock-doc-face))))))

  :init
  (ivy-rich-mode))


;; Editorconfig plugin for Emacs.
(use-package editorconfig
  :straight t
  :defer t
  :hook (prog-mode . editorconfig-mode))


;; Helpful package to quick share fragments of code with your teammates.
(use-package git-link
  :straight t
  :defer t

  :config
  ;; This might create a reference in the wrong lines if you're using branches
  ;; for development and your current version is too far ahead from origin.
  (setq git-link-default-branch "master"))

;; A simple command that takes a URL from the clipboard and inserts an org-mode
;; link with a title already.
(use-package org-cliplink
  :straight t
  :defer t)

;; A minor mode that builds a list of recently opened files.
(use-package recentf
  :no-require t
  :hook (kill-emacs . recentf-cleanup)

  :init
  (setq recentf-save-file (concat my/cache-dir "recentf")
        recentf-auto-cleanup 'never
        recentf-max-menu-items 0
        recentf-max-saved-items 200)

  :config
  ;; This hook should be in the config section because otherwise I get a
  ;; "function definition is void" error for the `recentf-add-file' function.
  (defun my/recentf-add-dired-directory ()
    "Add dired directory to recentf file list."
    (recentf-add-file default-directory))

  (add-hook 'dired-mode-hook #'my/recentf-add-dired-directory)

  (recentf-mode +1))

(provide 'my-packages-misc)
