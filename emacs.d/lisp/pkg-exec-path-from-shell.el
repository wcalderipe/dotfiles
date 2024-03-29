;;; pkg-exec-path-from-shell.el --- -*- lexical-binding: t; -*-

;;; Ensure environment variables inside Emacs look the same as in the user's
;;; shell.

(require 'use-package)

(use-package exec-path-from-shell
  :straight t
  :if (my/macos?)
  :config
  ;; Set JAVA_HOME to fix an error raised by cider during the execution of
  ;; cider-jack-in-clj(s).
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "JAVA_HOME"))

  ;; Fix an issue with tmux being auto started.
  ;; See https://github.com/purcell/exec-path-from-shell/issues/11#issuecomment-49550381
  (setq exec-path-from-shell-arguments '("-l"))

  (exec-path-from-shell-initialize))

(provide 'pkg-exec-path-from-shell)

;;; pkg-exec-path-from-shell.el ends here
