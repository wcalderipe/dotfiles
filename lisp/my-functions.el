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

(defun my/async-shell-command-no-window (command)
  "Execute string COMMAND asynchronously without opening buffer."
  (interactive "sAsync shell command: ")
  (let* ((buffer-name "*Async Shell Command*")
         (output-buffer (get-buffer-create buffer-name))
         (process (let ((display-buffer-alist (list (list buffer-name #'display-buffer-no-window))))
                    (async-shell-command command output-buffer)
                    (get-buffer-process output-buffer)))
         (sentinel `(lambda (process signal)
                      (when (memq (process-status process) '(exit signal))
                        (shell-command-sentinel process signal)
                        ;; Here you could run arbitrary code when the
                        ;; command is successful.
                        ;; (when (zerop (process-exit-status process))
                        ;;   (message "%s" ,cmd))
                        ))))
    (when (process-live-p process)
      (set-process-sentinel process sentinel))))

(defun my/projectile-run-async-shell-command-no-window-in-root ()
  "Invoke `my/async-shell-command-no-window' in the project's root."
  (interactive)
  (projectile-with-default-dir (projectile-ensure-project (projectile-project-root))
    (call-interactively 'my/async-shell-command-no-window)))

(defun my/el-get-is-git-package (package)
  (let ((default-directory (el-get-package-directory package)))
    (file-directory-p ".git")))

(defun my/el-get-git-most-recent-commit (package)
  "Fetch from remote the most recent git commit hash."
  (let ((default-directory (el-get-package-directory package)))
    (when (file-directory-p ".git")
      (let ((origin-url (s-chomp (shell-command-to-string "git config --get remote.origin.url"))))
        (-> "git ls-remote %s HEAD | awk '{ print $1 }'"
            (format origin-url)
            (shell-command-to-string)
            (s-chomp))))))

(defun my/el-get-installed-packages ()
  "Return all installed package names."
  (->> (el-get-package-status-alist)
       (-map 'car)
       (-filter 'el-get-package-installed-p)
       (-filter 'my/el-get-is-git-package)
       (-sort 'string<)))

(defun my/el-get-git-outdated-packages (packages)
  "Returns an alist of (<package-name> . <commit-hash>) for all
   outdated packages, i.e. where the remote commit is different than
   the local one installed by el-get.

   If you'd like to check on all installed packages:
   ;; => (my/el-get-git-outdated-packages (my/el-get-installed-packages))"
  (->> packages
       (-filter 'my/el-get-is-git-package)
       (-map (lambda (pkg)
               (let ((default-directory (el-get-package-directory pkg)))
                 (let* ((local-commit (s-chomp (shell-command-to-string "git rev-parse HEAD")))
                        (remote-commit (my/el-get-git-most-recent-commit pkg)))
                   (when (and remote-commit (not (string-equal local-commit remote-commit)))
                     `(,pkg . ,remote-commit))))))
       (-filter (-not 'null))))

(defun my/el-get-git-all-outdated-packages ()
  (my/el-get-git-outdated-packages (my/el-get-installed-packages)))

(defun my/dired-dotfiles-toggle ()
  "Show/hide dot-files"
  (interactive)
  (when (equal major-mode 'dired-mode)
    (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p) ; if currently showing
        (progn
          (set (make-local-variable 'dired-dotfiles-show-p) nil)
          (dired-mark-files-regexp "^\\\.")
          (dired-do-kill-lines))
      (progn (revert-buffer) ; otherwise just revert to re-show
             (set (make-local-variable 'dired-dotfiles-show-p) t)))))

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

(defun my/create-scratch-buffer nil
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (fundamental-mode))

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

   ;; Elixir mode
   ((eq major-mode 'elixir-mode)
    (alchemist-goto-definition-at-point))

   ;; Default jump to source strategy.
   (t (evil-jump-to-tag))))
