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


;; Distraction-free words correction with flyspell via Ivy.
(use-package flyspell-correct-ivy
  :straight t
  :after (ivy)
  :defer t
  :hook (flyspell-mode . my/enable-flyspell-correct-ivy)
  :preface
  (defun my/enable-flyspell-correct-ivy ()
    (interactive)
    (require 'flyspell-correct-ivy))

  :config
  (general-define-key
   :keymaps 'flyspell-mode-map
   "C-;" 'flyspell-correct-wrapper))


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


;; Utility package for jumping to visible text across windows and frames.
(use-package avy
  :straight t
  :defer t
  :config
  (general-define-key
   :states 'normal
   :keymaps 'override
   :prefix my/leader
   "aw" #'avy-goto-word-0
   "ac" #'avy-goto-char
   "al" #'avy-goto-line))


;; "jump to definition" package for 40+ languages.
(use-package dumb-jump
  :straight t
  :init
  ;; Use ivy instead of the default popup for multiple options.
  (setq dumb-jump-selector 'ivy)

  ;; When set to rg it will still use git-grep if it's a git project (because
  ;; it's the fastest), but will you use whatever you set here in any
  ;; other situation.
  (setq dumb-jump-prefer-searcher 'rg)

  ;; Adds dumb-jump to xref backend so I can use it with `M-.'.
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; A minor mode for dealing with pairs in Emacs.
(use-package smartparens
  :straight t
  :defer t
  :init
  ;; Only hooks with Lisp family major modes.
  (dolist (mode '(emacs-lisp-mode
                  common-lisp-mode
                  lisp-mode
                  clojure-mode
                  clojurec-mode
                  clojurescript-mode
                  cider-repl-mode))
    (let ((hook (intern (concat (symbol-name mode) "-hook"))))
      (add-hook hook #'smartparens-mode)))

  ;; Adds to IELM outside the dolist because it doesn't have a mode function.
  (add-hook 'ielm-mode-hook #'smartparens-mode))


;; Minor mode for performing structured editing of S-expression data.
(use-package paredit
  :straight t
  :init
  ;; Only hooks with Lisp family major modes.
  (dolist (mode '(emacs-lisp-mode
                  common-lisp-mode
                  lisp-mode
                  clojure-mode
                  clojurec-mode
                  clojurescript-mode
                  cider-repl-mode))
    (let ((hook (intern (concat (symbol-name mode) "-hook"))))
      (add-hook hook #'enable-paredit-mode)))

  ;; Adds to IELM outside the dolist because it doesn't have a mode function.
  (add-hook 'ielm-mode-hook #'enable-paredit-mode))


;; Folding code blocks based on indentation.
(use-package yafolding
  :straight t
  :defer t
  :hook (ruby-mode . yafolding-mode)
  :config
  (general-define-key
   :keymaps 'yafolding-mode-map
   "C-S-RET" #'yafolding-hide-parent-element
   "C-M-RET" #'yafolding-toggle-all
   "C-RET" #'yafolding-toggle-element))

(provide 'my-packages-misc)

;;; my-packages-misc ends here.
