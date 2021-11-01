;;; pkg-emacs-lisp-mode.el --- -*- lexical-binding: t; -*-

(require 'use-package)

(use-package emacs-lisp-mode
  :no-require t
  :config
  (defun pkg-emacs-lisp-mode/emacs-lisp-mode-hook ()
    (setq prettify-symbols-alist '(("lambda" . ?λ)))
    (prettify-symbols-mode 1))

  (defun pkg-emacs-lisp-mode/elisp-run-file-tests ()
    "Run all tests in the current buffer."
    (interactive)
    (let* ((base-name (file-name-base buffer-file-name))
           (prefix (progn
                     (string-match "^\\(.+-\\)test$" base-name)
                     (match-string 1 base-name))))
      (ert (concat "^" prefix))))

  (defun pkg-emacs-lisp-mode/elisp-run-project-tests ()
    "Run all tests prefixed with the current project name."
    (interactive)
    (let* ((root-path (projectile-project-root))
           (dir-name (file-name-nondirectory (directory-file-name root-path))))
      (ert (format "^%s-" dir-name))))

  (defun pkg-emacs-lisp-mode/pp-eval-defun-as-json-other-window ()
    "Pretty-print eval'ed JSON string in another buffer."
    (interactive)
    (let ((result (let ((inhibit-message t))
                    (elisp--eval-defun))))
      (with-current-buffer
          (switch-to-buffer-other-window "*Pretty-print JSON*")
        (read-only-mode -1)
        (erase-buffer)
        (insert result)
        (json-mode)
        (call-interactively #'json-pretty-print-buffer)
        (read-only-mode +1))))

  (defun pkg-emacs-lisp-mode/insert-pkg-boilerplate ()
    "Insert the code boilerplate to write a package in the current buffer."
    (interactive)
    (let* ((package-name (replace-regexp-in-string "\.el" "" (buffer-name))))
      (goto-char (point-max))
      (insert
       (format ";;; %s --- -*- lexical-binding: t; -*-

(require 'use-package)

(provide '%s)

;;; %s ends here"
               (buffer-name) package-name (buffer-name)))))

  ;; Disable elisp documentation warnings.
  (with-eval-after-load 'flycheck
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

  (add-hook 'emacs-lisp-mode-hook #'pkg-emacs-lisp-mode/emacs-lisp-mode-hook)

  (general-define-key
   :prefix my/mode-leader
   :states 'normal
   :keymaps 'emacs-lisp-mode-map
   ;; Eval
   "e e" #'eval-defun
   "e l" #'eval-last-sexp
   "e P" #'pp-eval-last-sexp

   ;; Test
   "t f" #'pkg-emacs-lisp-mode/elisp-run-file-tests
   "t p" #'pkg-emacs-lisp-mode/elisp-run-project-tests

   ;; Code
   "c i p" #'pkg-emacs-lisp-mode/insert-pkg-boilerplate))

(provide 'pkg-emacs-lisp-mode)

;;; pkg-emacs-lisp-mode.el ends here
