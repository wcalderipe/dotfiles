(use-package js2-mode
  :pin MELPA
  :mode (("\\.js\\'" . js2-mode)
         ("\\.mjs\\'" . js2-mode))

  :config
  (use-package xref-js2
    :pin MELPA
    :config
    (define-key js2-mode-map (kbd "M-.") nil)
    (add-hook 'js2-mode-hook
              (lambda () (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))
  (setq-default
   ;js2-line-break t
   js2-basic-offset 2
   js2-bounce-indent-p nil
   js2-enter-indents-newline t ; don't need to press tab before typing
   js2-indent-on-enter-key t ; fix indenting before moving on

   ;; Disable all parse errors and warnings by default,
   ;; leaving room for flycheck to handle them.
   js2-mode-show-parse-errors nil
   js2-mode-show-strict-warnings nil

   ;; Disable warnings about missing semi-colons.
   ;; js2-missing-semi-one-line-override t
   ;; js2-strict-missing-semi-warning nil

   ;; Adds highlighting of many Ecma built-in functions.
   js2-highlight-level 3

   js2-global-externs (list "afterAll"
                            "afterEach"
                            "beforeAll"
                            "beforeEach"
                            "context"
                            "describe"
                            "it")))

(provide 'javascript.init)
;;; javascript.init.el ends here
