(use-package emacs-lisp-mode
  :no-require t

  :config
  (defun my/emacs-lisp-mode-hook ()
    (setq prettify-symbols-alist '(("lambda" . ?λ)))
    (prettify-symbols-mode 1))

  (defun my/elisp-run-file-tests ()
    "Run all tests in the current buffer."
    (interactive)
    (let* ((base-name (file-name-base buffer-file-name))
           (prefix (progn
                     (string-match "^\\(.+-\\)test$" base-name)
                     (match-string 1 base-name))))
      (ert (concat "^" prefix))))

  (defun my/elisp-run-project-tests ()
    "Run all tests prefixed with the current project name."
    (interactive)
    (let* ((root-path (projectile-project-root))
           (dir-name (file-name-nondirectory (directory-file-name root-path))))
      (ert (format "^%s-" dir-name))))

  (defun my/pp-eval-defun-as-json-other-window ()
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

  (add-hook 'emacs-lisp-mode-hook #'my/emacs-lisp-mode-hook)

  (general-define-key
   :prefix my/leader
   :states 'normal
   :keymaps 'emacs-lisp-mode-map
   "e" #'eval-defun
   "E" #'eval-last-sexp
   "P" #'pp-eval-last-sexp
   "tf" #'my/elisp-run-file-tests
   "tp" #'my/elisp-run-project-tests))

;; Emulates Surround.vim for Evil. Everything about "surroundings":
;; parentheses, brackets, quotes, XML tags, and more.
(use-package evil-surround
  :straight t

  :after (evil)

  :defer t

  :init
  (add-hook 'prog-mode-hook #'global-evil-surround-mode)

  :config
  (add-to-list 'evil-surround-operator-alist '(evil-cp-change . change))
  (add-to-list 'evil-surround-operator-alist '(evil-cp-delete . delete)))

;; Comment/uncomment lines efficiently. Like Nerd Commenter in Vim.
(use-package evil-nerd-commenter
  :straight t

  :after (evil)

  :commands (evilnc-comment-or-uncomment-lines)

  :init
  ;; Improved toggle comment/uncomment lines.
  (general-define-key
   "M-;" #'evilnc-comment-or-uncomment-lines))

(provide 'my-packages-languages)
