(defun my/xml-pretty-print-buffer ()
  (interactive)
  (save-excursion
    (sgml-mode)
    (sgml-pretty-print (point-min) (point-max))
    (indent-region (point-min) (point-max))))

(defun my/reload-init-file ()
  (interactive)
  (load-file user-init-file))

(defun my/indent-or-complete ()
  (interactive)
  (if (looking-at "\\_>")
    (company-complete-common)
    (indent-according-to-mode)))

(defun my/use-read-only-mode-in-shell-command-buffers ()
  "I usually use shell command buffers to read text, but not change
  it. For example, the *Async Shell Command* buffer is created when
  the projectile-run-async-shell-command-in-root function is called."
  (when-let ((buffer (get-buffer "*Async Shell Command*")))
    (with-current-buffer buffer
      (read-only-mode))))

(defun my/use-eslint-from-node-modules ()
  "Use local eslint from node_modules before global.
  http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable"
  (let* ((root (locate-dominating-file
                 (or (buffer-file-name) default-directory)
                 "node_modules"))
          (eslint (and root (expand-file-name "node_modules/eslint/bin/eslint.js"
                              root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(defun my/counsel-projectile-switch-project-action-dired (project)
  "Open dired when switching projects with counsel-projectile."
  (let ((projectile-switch-project-action
          (lambda ()
            (projectile-dired))))
    (counsel-projectile-switch-project-by-name project)))

(defun my/cider-format-and-clean-buffer ()
  "Format Clojure code in the current buffer and clean the ns form."
  (interactive)
  (cljr-clean-ns)
  (cider-format-buffer))

(defun my/cider-append-comment ()
  "Utility function to print result to buffer as comment."
  (when (null (nth 8 (syntax-ppss)))
    (insert " ; ")))

(defun my/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
    (delq (current-buffer)
      (remove-if-not 'buffer-file-name (buffer-list)))))

(defun my/vim-split ()
  "Splits the current window horizontally and switch to the new window."
  (interactive)
  (evil-window-split)
  (evil-window-down 1))

(defun my/vim-vsplit ()
  "Splits the current window vertically and switch to the new window."
  (interactive)
  (evil-window-vsplit)
  (evil-window-right 1))

(defun my/jump-to-source ()
  "Jump to source as if using ctags."
  (interactive)
  (cond ;; Clojure mode.
    ((eq major-mode 'clojure-mode)
      (cider-find-var))

    ;; Javascript mode (requires xref-js2 package to work) or Elisp
    ;; mode.
    ((or (eq major-mode 'js2-mode)
       (eq major-mode 'emacs-lisp-mode))
      (xref-find-definitions (symbol-name (symbol-at-point))))

    ;; TypeScript mode.
    ((eq major-mode 'typescript-mode)
      (tide-jump-to-definition))

    ;; Rust mode
    ((eq major-mode 'rust-mode)
      (racer-find-definition))

    ;; Default jump to source strategy.
    (t (evil-jump-to-tag))))
