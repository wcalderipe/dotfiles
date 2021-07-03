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


;; The Clojure Interactive Development Environment that Rocks.
(use-package cider
  :straight t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode))
  :init
  (setq cider-auto-select-error-buffer nil
        cider-auto-select-test-report-buffer nil
        cider-repl-display-help-banner nil
        cider-repl-history-size 1000
        cider-repl-wrap-history t
        cider-show-error-buffer 'except-in-repl)

  ;; Prevent 'cider-load-buffer from prompting to save the file
  ;; corresponding to the buffer being loaded, if it's modified.
  (setq cider-save-file-on-load t)

  ;; Do not prompt for symbol confirmation (e.g. show docs and jump to
  ;; definition without asking).
  (setq cider-prompt-for-symbol nil)

  ;; Do not open CIDER buffer after successfull connection to REPL.
  (setq cider-repl-pop-to-buffer-on-connect nil)

  ;; Applies syntax high-light to Clojure evaluated code overlay.
  (setq cider-overlays-use-font-lock t)

  ;; CIDER can colorize usages of functions and variables from any namespace,
  ;; not only macros and core Clojure functions.
  (setq cider-font-lock-dynamically '(macro core function var))

  ;; Use Fast Idiomatic Pretty Printer (5-10x faster than clojure.core/pprint).
  (setq cider-repl-use-pretty-printing t)

  ;; Removes evaluation fringe indicators - it's distracting.
  (setq cider-use-fringe-indicators nil)

  ;; Subword minor mode is useful for Java/Javascript interop.
  (add-hook 'cider-repl-mode-hook #'subword-mode)
  (add-hook 'cider-repl-mode-hook #'smartparens-mode)

  (defun my/cider--reload-user-namespace ()
    (interactive)
    (cider-interactive-eval "(require 'user :reload) \"Namespace 'user reloaded.\""))

  (defun my/cider--trailing-whitespace ()
    (setq show-trailing-whitespace nil))

  ;; Disable annoying trailing whitespace warnings.
  (add-hook 'cider-repl-mode-hook #'my/cider--trailing-whitespace)
  (add-hook 'cider-test-report-mode-hook #'my/cider--trailing-whitespace)

  :config
  ;; Indentation settings
  (define-clojure-indent
    (prop/for-all 1))

  (with-eval-after-load 'evil
    ;; Add to jump list, i.e. record location prior to running commands.
    (evil-add-command-properties #'cider-test-run-project-tests :jump t))

  (general-define-key
   :prefix my/leader
   :states 'normal
   :keymaps 'clojure-mode-map
   "d" #'cider-doc
   "e" #'cider-eval-defun-at-point
   "E" #'cider-eval-last-sexp
   "p" #'cider-pprint-eval-defun-at-point
   "P" #'cider-pprint-eval-last-sexp
   "r" #'cider-load-all-project-ns
   "s" #'cider-browse-spec
   "u" #'my/cider--reload-user-namespace
   "ta" #'cider-test-run-project-tests
   "tf" #'cider-test-run-ns-tests
   "tl" #'cider-test-rerun-test
   "tt" #'cider-test-run-test))


;; Defines several hydras for CIDER.
(use-package cider-hydra
  :straight t
  :defer t
  :after (cider)
  :hook ((clojure-mode clojurescript-mode) . cider-hydra-mode))


(use-package ruby-mode
  :config
  ;; Don't insert file enconding comment at the top.
  (setq ruby-insert-encoding-magic-comment nil)

  ;; Don't indent a function args aligned with the opening bracket.
  (setq ruby-deep-indent-paren nil)

  (dolist (file-pattern '("\\.rb$"
                          "\\.rake$"
                          "Rakefile$"
                          "\\.gemspec$"
                          "\\.ru$"
                          "Gemfile$"))
    (add-to-list 'auto-mode-alist (cons file-pattern 'ruby-mode))))


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
  :defer t
  :init
  (defun my/rspec-toggle-use-docker ()
    "Toggles `rspec-use-docker-when-possible' value (`nil' or `t')."
    (interactive)
    (setq rspec-use-docker-when-possible (not rspec-use-docker-when-possible))
    (message "Toggle rspec-use-docker-when-possible value to `%s'"
             rspec-use-docker-when-possible))

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


;; Simple interface for RuboCop (Ruby linter).
;; It requires RuboCop gem insalled globally or bundled in a project.
(use-package rubocop
  :straight t
  :defer t
  :hook (ruby-mode . rubocop-mode)
  :config
  (general-define-key
    :prefix my/leader
    :states 'normal
    :keymaps 'rubocop-mode-map
    "la" 'rubocop-autocorrect-project
    "lf" 'rubocop-autocorrect-current-file
    "lca" 'rubocop-check-project
    "lcf" 'rubocop-check-current-file))


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


;; A simple language mode for the Solidity.
(use-package solidity-mode
  :straight t
  :defer t
  :mode "\\.sol\\'")


(use-package markdown-mode
  :straight t
  :defer t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :hook (markdown-mode . flyspell-mode)
  :init
  ;; Sets the command which will open and preview files.
  (setq markdown-command "markdown"))


(use-package plantuml-mode
  :straight t
  :defer t
  :mode ("\\.puml\\'" . plantuml-mode)
  :init
  (setq plantuml-executable-path "~/.local/bin/plantuml")
  (setq plantuml-default-exec-mode 'executable)

  ;; org-babel uses the jar path instead of the executable.
  (setq org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar"))


(use-package web-mode
  :straight t
  :defer t
  :mode "\\.\\(?:\\(?:h\\(?:bs\\|tml\\)\\|liquid\\|tmpl\\)\\)\\'"
  :init
  (setq web-mode-markup-indent-offset 2)

  :config
  (general-define-key
    :keymaps 'evil-normal-state-map
    [remap evil-toggle-fold] #'web-mode-fold-or-unfold))


;; TypeScript Interactive Development Environment.
(use-package tide
  :straight t
  :defer t
  :hook (typescript-mode . tide-setup)
  :mode ("\\.\\(ts\\|tsx\\)\\'" . typescript-mode)
  :config
  (add-to-list 'company-backends '(company-tide company-files)))


;; Emulates Surround.vim for Evil. Everything about "surroundings":
;; parentheses, brackets, quotes, XML tags, and more.
(use-package evil-surround
  :straight t
  :defer t
  :after (evil)
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

;;; my-packages-languages ends here.
