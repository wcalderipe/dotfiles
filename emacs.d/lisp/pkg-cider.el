;;; pkg-cider.el --- -*- lexical-binding: t; -*-

;;; The Clojure Interactive Development Environment that Rocks.

(require 'use-package)

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

  ;; Do not save the file on load. Eval is one thing, saving a file is another
  ;; with potential side-effects. This is specially important when developing
  ;; with hot reload (e.g. Shadow CLJS). ELisp also doesn't save the file when
  ;; `eval-buffer' is called.
  (setq cider-save-file-on-load nil)

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

  (defun pkg-cider/cider--reload-user-namespace ()
    (interactive)
    (cider-interactive-eval "(require 'user :reload) \"Namespace 'user reloaded.\""))

  (defun pkg-cider/cider--trailing-whitespace ()
    (setq show-trailing-whitespace nil))

  ;; Disable annoying trailing whitespace warnings.
  (add-hook 'cider-repl-mode-hook #'pkg-cider/cider--trailing-whitespace)
  (add-hook 'cider-test-report-mode-hook #'pkg-cider/cider--trailing-whitespace)

  ;; For some reason, define keys in the `:config' scope doesn't work.
  (general-define-key
   :prefix my/mode-leader
   :states 'normal
   :keymaps 'clojure-mode-map
   ;; Eval
   "e L"   #'cider-load-all-project-ns
   "e P"   #'cider-pprint-eval-last-sexp
   "e b"   #'cider-eval-buffer
   "e e"   #'cider-eval-defun-at-point
   "e f"   #'cider-load-file
   "e i"   #'cider-inspect-last-sexp
   "e l"   #'cider-eval-last-sexp
   "e n"   #'cider-eval-ns-form
   "e p"   #'cider-pprint-eval-defun-at-point
   "e r"   #'cider-load-all-project-ns
   ;; Help
   "h d"   #'cider-doc
   "h a"   #'cider-apropos
   "h c"   #'cider-clojuredocs
   ;; Inspect
   "i ."   #'cider-inspect
   "i r"   #'cider-inspect-last-result
   ;; Macroexpand
   "m m"   #'cider-macroexpand-1
   "m M"   #'cider-macroexpand-all
   ;; Namespace
   "n l l" #'cider-ns-reload
   "n l a" #'cider-ns-reload-all
   "n b b" #'cider-browse-ns
   "n b a" #'cider-browse-ns-all
   "n r"   #'cider-ns-refresh
   ;; Debug
   "d f"   #'cider-debug-defun-at-point
   ;; REPL
   "r C"   #'cider-connect-clj
   "r R"   #'cider-restart
   "r b"   #'cider-switch-to-repl-buffer
   "r c"   #'cider-repl-clear-buffer
   "r n"   #'cider-repl-set-ns
   "r s"   #'cider-jack-in
   "r x"   #'cider-quit
   ;; Test
   "t f"   #'cider-test-run-ns-tests
   "t l"   #'cider-test-rerun-test
   "t n"   #'cider-test-run-ns
   "t p"   #'cider-test-run-project-tests
   "t t"   #'cider-test-run-test)

  (general-define-key
   :prefix my/mode-leader
   :states 'normal
   :keymaps 'clojurescript-mode-map
   ;; REPL
   "r C"   #'cider-connect-cljs)

  :config
  ;; Indentation settings
  (define-clojure-indent
    (prop/for-all 1))

  (with-eval-after-load 'evil
    ;; Add to jump list, i.e. record location prior to running commands.
    (evil-add-command-properties #'cider-test-run-project-tests :jump t)))

(provide 'pkg-cider)

;;; pkg-cider.el ends here
