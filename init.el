(defun my/auto-install-package-manager ()
  (add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
  (unless (require 'el-get nil 'noerror)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
      (goto-char (point-max))
      (eval-print-last-sexp))))

(defun my/install-packages ()
  ;; Configure directory where all init-* files run initialization code
  ;; after each package has been installed by el-get.
  (setq el-get-user-package-directory (locate-user-emacs-file "init"))
  (load (locate-user-emacs-file "packages.el")))

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; First load basic configurations that don't depend on extra packages.
(load (locate-user-emacs-file "lisp/my-basic-behavior.el"))
(load (locate-user-emacs-file "lisp/init-dired-ls-lisp.el"))

(my/auto-install-package-manager)
(my/install-packages)

;; Load other customizations.
(load (locate-user-emacs-file "lisp/my-functions.el"))
(load (locate-user-emacs-file "lisp/init-modes.el"))
(load (locate-user-emacs-file "lisp/my-keybindings.el"))
(load (locate-user-emacs-file "lisp/init-shell-mode.el"))

(when (file-exists-p custom-file)
  (load custom-file))
