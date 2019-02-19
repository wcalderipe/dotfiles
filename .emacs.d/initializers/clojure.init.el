(use-package clojure-mode
  :mode "(\\.clj|\\.cljs)$")

(use-package clj-refactor
  :pin MELPA
  :mode "(\\.clj|\\.cljs)$")

;; https://github.com/technomancy/slamhound
(use-package cider
  :mode "(\\.clj|\\.cljs)$"
  :pin MELPA

  :init
  (setq
   cider-auto-select-error-buffer nil
   cider-auto-select-test-report-buffer nil
   cider-repl-display-help-banner nil
   cider-repl-history-size 1000
   cider-repl-wrap-history t
   cider-show-error-buffer 'except-in-repl

   ;; Instruct CIDER to use Figwheel (use cider-jack-in-clojurescript).
   cider-cljs-lein-repl "(do (require 'figwheel-sidecar.repl-api)
                             (figwheel-sidecar.repl-api/start-figwheel!)
                             (figwheel-sidecar.repl-api/cljs-repl))"

   ;; Prevent 'cider-load-buffer from prompting to save the file
   ;; corresponding to the buffer being loaded, if it's modified.
   cider-save-file-on-load t

   ;; Hide the *nrepl-connection* and *nrepl-server* buffers from
   ;; appearing in some buffer switching commands like
   ;; switch-to-buffer. Useful for debugging Cider
   ;; nrepl-hide-special-buffers t

   ;; Do not prompt for symbol confirmation.
   ;; E.g. show docs and jump to definition without
   ;; asking.
   cider-prompt-for-symbol nil

   ;; Do not open CIDER buffer after successfull
   ;; connection to REPL.
   cider-repl-pop-to-buffer-on-connect nil

   ;; When you evaluate code in Clojure files, the result is
   ;; displayed in the buffer itself, in an overlay right after
   ;; the evaluated code. If you want this overlay to be
   ;; font-locked (syntax-highlighted) like Clojure code
   cider-overlays-use-font-lock t

   ;; CIDER can colorize usages of functions and
   ;; variables from any namespace, not only macros and
   ;; core Clojure functions.
   cider-font-lock-dynamically '(macro core function var)

   ;; Use Fast Idiomatic Pretty Printer (5-10x faster
   ;; than clojure.core/pprint)
   cider-repl-use-pretty-printing t
   cider-pprint-fn 'puget)

  (defun my/format-and-clean-buffer ()
    "Format Clojure code in the current buffer and clean the ns form."
    (interactive)
    (cljr-clean-ns)
    (cider-format-buffer))

  ;; Subword minor mode is useful for Java/Javascript interop.
  (add-hook 'cider-repl-mode-hook 'subword-mode)
  (add-hook 'cider-repl-mode-hook 'smartparens-mode)

  ;; Disable annoying trailing whitespace warnings.
  (add-hook 'cider-repl-mode-hook '(lambda () (setq show-trailing-whitespace nil)))
  (add-hook 'cider-test-report-mode-hook '(lambda () (setq show-trailing-whitespace nil)))

  ;; Utility function to print result to buffer as comment.
  (defun ha/cider-append-comment ()
    (when (null (nth 8 (syntax-ppss)))
      (insert " ; ")))
  (advice-add 'cider-eval-print-last-sexp :before #'ha/cider-append-comment)

  :config
  (use-package helm-cider
    :pin MELPA
    :mode "(\\.clj|\\.cljs)$"
    :config (helm-cider-mode 1)))

(provide 'clojure.init)
;;; clojure.init.el ends here
