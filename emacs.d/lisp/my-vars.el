(defvar bootstrap-version nil
  "Used by the straight package manager.")

(defconst my/emacs-dir
  (eval-when-compile (file-truename user-emacs-directory))
  "The path to the currently loaded .emacs.d directory. Must end with a slash.")

(defconst my/local-dir
  (concat my/emacs-dir ".local/")
  "Root directory for local storage. Must end with a slash.")

(defconst my/cache-dir (concat my/local-dir "cache/")
  "Directory for volatile local storage. Must end with a slash.")

(defconst my/leader "SPC"
  "Global prefix used in `general' keybindings.")

(provide 'my-vars)
