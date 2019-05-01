;; For some reason we must manually require cider-find in order to
;; enable jump to definition. In previous versions of cider (version
;; < 0.21.x) this wasn't necessary.
(require 'cider-find)

(setq cider-auto-select-error-buffer nil)
(setq cider-auto-select-test-report-buffer nil)
(setq cider-repl-display-help-banner nil)
(setq cider-repl-history-size 1000)
(setq cider-repl-wrap-history t)
(setq cider-show-error-buffer 'except-in-repl)

;; Instruct CIDER to use Figwheel (use cider-jack-in-clojurescript).
(setq cider-cljs-lein-repl "(do (require 'figwheel-sidecar.repl-api)
                              (figwheel-sidecar.repl-api/start-figwheel!)
                              (figwheel-sidecar.repl-api/cljs-repl))")

;; Prevent 'cider-load-buffer from prompting to save the file
;; corresponding to the buffer being loaded, if it's modified.
(setq cider-save-file-on-load t)

;; Hide the *nrepl-connection* and *nrepl-server* buffers from
;; appearing in some buffer switching commands like
;; switch-to-buffer. Useful for debugging Cider
;; (setq nrepl-hide-special-buffers t)

;; Do not prompt for symbol confirmation.
;; E.g. show docs and jump to definition without
;; asking.
(setq cider-prompt-for-symbol nil)

;; Do not open CIDER buffer after successfull
;; connection to REPL.
(setq cider-repl-pop-to-buffer-on-connect nil)

;; When you evaluate code in Clojure files, the result is
;; displayed in the buffer itself, in an overlay right after
;; the evaluated code. If you want this overlay to be
;; font-locked (syntax-highlighted) like Clojure code
(setq cider-overlays-use-font-lock t)

;; CIDER can colorize usages of functions and
;; variables from any namespace, not only macros and
;; core Clojure functions.
(setq cider-font-lock-dynamically '(macro core function var))

;; Use Fast Idiomatic Pretty Printer (5-10x faster
;; than clojure.core/pprint)
(setq cider-repl-use-pretty-printing t)
(setq cider-pprint-fn 'puget)

;; Subword minor mode is useful for Java/Javascript interop.
(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'smartparens-mode)

;; Disable annoying trailing whitespace warnings.
(add-hook 'cider-repl-mode-hook '(lambda () (setq show-trailing-whitespace nil)))
(add-hook 'cider-test-report-mode-hook '(lambda () (setq show-trailing-whitespace nil)))

;; Add to jump list, i.e. record location prior to running commands.
(evil-add-command-properties #'cider-test-run-project-tests :jump t)

(advice-add 'cider-eval-print-last-sexp :before #'my/cider-append-comment)
