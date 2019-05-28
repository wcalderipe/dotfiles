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

(defun my/load-lisp-init-files ()
  (let* ((this-dir (f-dirname (f-this-file)))
         (init-files (f-glob "init-*.el" (f-join this-dir "lisp"))))
    (mapcar 'load init-files)))

(defun my/init-emacs ()
  (when load-file-name
    (setq user-emacs-directory (file-name-directory load-file-name)))

  ;; First load basic configurations that don't depend on extra packages.
  (load (locate-user-emacs-file "lisp/my-basic-behavior.el"))
  (load (locate-user-emacs-file "lisp/my-functions.el"))

  (my/auto-install-package-manager)
  (my/install-packages)

  ;; Load other customizations and lastly the keybindings.
  (my/load-lisp-init-files)
  (load (locate-user-emacs-file "lisp/my-keybindings.el"))

  (when (file-exists-p custom-file)
    (load custom-file)))

(my/init-emacs)