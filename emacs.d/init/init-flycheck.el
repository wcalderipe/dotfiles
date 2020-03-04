;; Whitelist major modes with flycheck enabled.
;; Currently enabled for Elisp, Javascript and TypeScript.
(setq  flycheck-global-modes '(js2-mode
                                tide-mode))

;; Flycheck should create hidden temporary files.
(setq  flycheck-temp-prefix ".flycheck")

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

;; Flycheck slow when opening JS files while waiting for eslint.
;; https://github.com/flycheck/flycheck/issues/1129
(advice-add 'flycheck-eslint-config-exists-p :override (lambda() t))

(global-flycheck-mode)
