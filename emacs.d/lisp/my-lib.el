(defun my/install-package-manager ()
  (let ((bootstrap-file
	 (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
	(bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
	  (url-retrieve-synchronously
	   "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	   'silent 'inhibit-cookies)
	(goto-char (point-max))
	(eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  ;; Auto-install `use-package'.
  (straight-use-package 'use-package))

(defun my/remove-horizontal-scroll-margin-in-shells ()
  "Remove scroll margin to prevent jumpiness in shell(s) mode."
  (setq-local hscroll-margin 0))

(defun my/copy-file-name-to-clipboard ()
  "Copy the current buffer absolute file name to the clipboard."
  (interactive)
  (let ((file-name (if (equal major-mode 'dired-mode)
		      default-directory
		    (buffer-file-name))))
    (when file-name
      (kill-new file-name)
      (message "Copied buffer file name '%s' to the clipboard." file-name))))

(provide 'my-lib)
