;; Modular completion framework.
(use-package company
  :straight t
  :hook (after-init . global-company-mode)
  :commands (company-complete-common
              company-manual-begin
              company-grab-line))


(use-package emacs-lisp-mode
  :no-require t

  :config
  (defun my/emacs-lisp-mode-hook ()
    (setq prettify-symbols-alist '(("lambda" . ?λ)))
    (prettify-symbols-mode 1))

  (defun my/elisp-run-file-tests ()
    "Run all tests in the current buffer."
    (interactive)
    (let* ((base-name (file-name-base buffer-file-name))
            (prefix (progn
                      (string-match "^\\(.+-\\)test$" base-name)
                      (match-string 1 base-name))))
      (ert (concat "^" prefix))))

  (defun my/elisp-run-project-tests ()
    "Run all tests prefixed with the current project name."
    (interactive)
    (let* ((root-path (projectile-project-root))
            (dir-name (file-name-nondirectory (directory-file-name root-path))))
      (ert (format "^%s-" dir-name))))

  (defun my/pp-eval-defun-as-json-other-window ()
    "Pretty-print eval'ed JSON string in another buffer."
    (interactive)
    (let ((result (let ((inhibit-message t))
                    (elisp--eval-defun))))
      (with-current-buffer
        (switch-to-buffer-other-window "*Pretty-print JSON*")
        (read-only-mode -1)
        (erase-buffer)
        (insert result)
        (json-mode)
        (call-interactively #'json-pretty-print-buffer)
        (read-only-mode +1))))

  (add-hook 'emacs-lisp-mode-hook #'my/emacs-lisp-mode-hook)

  (general-define-key
    :prefix my/leader
    :states 'normal
    :keymaps 'emacs-lisp-mode-map
    "e" #'eval-defun
    "E" #'eval-last-sexp
    "P" #'pp-eval-last-sexp
    "tf" #'my/elisp-run-file-tests
    "tp" #'my/elisp-run-project-tests))


(use-package org
  :straight t
  :commands (org-mode)

  :preface
  ;; Stores ID locations in the cache directory.
  (setq org-id-locations-file (concat my/cache-dir "org-id-locations"))

  ;; Removes footnote HTML validation link.
  (setq org-html-validation-link nil)

  ;; Opens org files with all headlines visible.
  (setq org-startup-folded 'showall)

  ;; Records a timestamp when a todo item is DONE.
  (setq org-log-done 'time)

  ;; Place tags directly after headline text, with only one space in between.
  (setq org-tags-column 0)

  (setq org-todo-keywords
        '((sequence
           "TODO"
           "DONE")))

  :config
  ;; Disables auto indentation in BEGIN blocks. Let me handle it.
  (add-hook 'org-mode-hook (lambda () (electric-indent-mode -1)))

  ;; Languages which can be evaluated in Org buffers.
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t))))

;; Supplemental evil-mode key-bindings to org-mode.
(use-package evil-org
  :straight t

  :after
  (org evil)

  :commands
  (evil-org evil-org-agenda)

  :init
  (add-hook 'org-mode-hook 'evil-org-mode))


(use-package ruby-mode
  :config
  ;; Don't insert file enconding comment at the top.
  (setq ruby-insert-encoding-magic-comment nil)

  ;; Don't indent a function args aligned with the opening bracket.
  (setq ruby-deep-indent-paren nil)

  (add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode)))


(use-package inf-ruby
  :straight t
  :defer t
  :hook ((after-init . inf-ruby-switch-setup))
  :config
  (general-define-key
   :prefix my/leader
   :states 'normal
   :keymaps 'rspec-compilation-mode-map
   ;; Switches from rspec-mode to inf-ruby-mode so I can interact with Pry or
   ;; byebug.
   "c" 'inf-ruby-switch-from-compilation))


;; Provides some convenience functions for dealing with RSpec.
(use-package rspec-mode
  :straight t
  :init
  (defun my/rspec-toggle-use-docker ()
    "Toggles `rspec-use-docker-when-possible' value (`nil' or `t')."
    (interactive)
    (setq rspec-use-docker-when-possible (not rspec-use-docker-when-possible))
    (message "Toggle rspec-use-docker-when-possible value to `%s'" rspec-use-docker-when-possible))

  (setq rspec-use-docker-when-possible nil)

  ;; Sets the command to `exec' because the projects I work with the test
  ;; container is always running in the background.
  (setq rspec-docker-command "docker-compose exec")

  ;; Sets the container name.
  (setq rspec-docker-container "test")

  ;; Sets docker cwd to empty because containers workdir are often set to the
  ;; project root.
  (setq rspec-docker-cwd "")

  :config
  (general-define-key
    :prefix my/leader
    :states 'normal
    :keymaps 'ruby-mode-map
    "ta" 'rspec-verify-all
    "tf" 'rspec-verify
    "tl" 'rspec-rerun
    "tt" 'rspec-verify-single))


(use-package dockerfile-mode
  :straight t
  ;; Any file starting with "Dockerfile" should enable this mode.
  :mode (("^Dockerfile" . dockerfile-mode)))


(use-package yaml-mode
  :straight t
  :mode "\\.ya?ml\\'")


(use-package csv-mode
  :straight t
  :defer t)


(use-package json-mode
  :straight t
  :defer t)


(use-package graphql-mode
  :straight t
  :defer t)


(use-package markdown-mode
  :straight t
  :mode (("\\.md\\'" . markdown-mode)
          ("\\.markdown\\'" . markdown-mode))

  :init
  ;; Sets the command which will open and preview files.
  (setq markdown-command "markdown"))


(use-package plantuml-mode
  :straight t
  :mode ("\\.puml\\'" . plantuml-mode)
  :init
  (setq plantuml-executable-path "~/.local/bin/plantuml")
  (setq plantuml-default-exec-mode 'executable)

  ;; org-babel uses the jar path instead of the executable.
  (setq org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar"))


(use-package web-mode
  :straight t
  :mode "\\.\\(?:\\(?:h\\(?:bs\\|tml\\)\\|liquid\\|tmpl\\)\\)\\'"
  :init
  (setq web-mode-markup-indent-offset 2)

  :config
  (general-define-key
    :keymaps 'evil-normal-state-map
    [remap evil-toggle-fold] #'web-mode-fold-or-unfold))


(use-package tide
  :straight t
  :hook (typescript-mode . tide-setup)
  :mode ("\\.\\(ts\\|tsx\\)\\'" . typescript-mode)
  :config
  (add-to-list 'company-backends '(company-tide company-files)))


(use-package js2-mode
  :straight t
  :mode "\\.js\\'"
  :init
  (setq js-indent-level 2
    js2-bounce-indent-p nil)

  ;; Disable all parse errors and warnings by default,
  ;; leaving room for flycheck to handle them.
  (setq js2-mode-show-parse-errors nil
    js2-mode-show-strict-warnings nil
    js2-strict-missing-semi-warning nil)

  ;; Adds highlighting of many Ecma built-in functions.
  (setq js2-highlight-level 3))


;; Emulates Surround.vim for Evil. Everything about "surroundings":
;; parentheses, brackets, quotes, XML tags, and more.
(use-package evil-surround
  :straight t

  :after (evil)

  :defer t

  :init
  (add-hook 'prog-mode-hook #'global-evil-surround-mode)

  :config
  (add-to-list 'evil-surround-operator-alist '(evil-cp-change . change))
  (add-to-list 'evil-surround-operator-alist '(evil-cp-delete . delete)))


;; Comment/uncomment lines efficiently. Like Nerd Commenter in Vim.
(use-package evil-nerd-commenter
  :straight t

  :after (evil)

  :commands (evilnc-comment-or-uncomment-lines)

  :init
  ;; Improved toggle comment/uncomment lines.
  (general-define-key
    "M-;" #'evilnc-comment-or-uncomment-lines))

(provide 'my-packages-languages)
