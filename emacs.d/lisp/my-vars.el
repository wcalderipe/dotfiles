(defvar bootstrap-version nil
  "Used by the straight package manager.")

(defconst my/emacs-dir
  (eval-when-compile (file-truename user-emacs-directory))
  "The path to the currently loaded .emacs.d directory. Must end with a slash.")

(defconst my/org-dir
  "~/org/"
  "Root directory for Org files.")

(defconst my/local-dir
  (concat my/emacs-dir ".local/")
  "Root directory for local storage. Must end with a slash.")

(defconst my/cache-dir (concat my/local-dir "cache/")
  "Directory for volatile local storage. Must end with a slash.")

(defconst my/leader "SPC"
  "Global prefix used in `general' keybindings.")

(defconst my/mode-leader "m"
  "Mode prefix used in `general' keybinding of a specific Emacs mode. It's inspired by Vim's leader and local leader.")

(defconst my/color-cyan "#93E0E3")
(defconst my/color-gray "#5F5F5F")

(defconst my/gui? (display-graphic-p))

(defconst my/lisp-modes '(emacs-lisp-mode
                          common-lisp-mode
                          lisp-mode
                          lisp-interaction-mode
                          clojure-mode
                          clojurec-mode
                          clojurescript-mode
                          cider-repl-mode))

(provide 'my-vars)

;;; my-vars ends here.
