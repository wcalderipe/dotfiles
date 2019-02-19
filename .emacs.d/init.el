(require 'package)

;; Affects how the load function chooses the file to load.
;; Never accidentally use outdated compiled files.
(setq load-prefer-newer t)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("MELPA Stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("GNU ELPA" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("MELPA" . "https://melpa.org/packages/"))

;; Prefer MELPA Stable over GNU over MELPA. IOW prefer MELPA's stable
;; packages over everything and only fall back to GNU or MELPA if
;; necessary.
(setq package-archive-priorities
      '(("MELPA Stable" . 10)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 0)))
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-verbose t)
(setq use-package-always-ensure t)
;; To further reduce load time (upgrading to use-package 2.x).
(eval-when-compile
  (require 'use-package))
(require 'diminish) ;; If you use :diminish
(require 'bind-key) ;; If you use any :bind variant

;; Customize where auto generated settings should be stored.
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Add a directory to our load path so that when you `require` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path (locate-user-emacs-file "initializers"))
(add-to-list 'load-path (locate-user-emacs-file "themes"))
(add-to-list 'custom-theme-load-path (locate-user-emacs-file "themes"))


;; ========
;; Packages
;; ========

(require 'editor.init)
(require 'evil.init)
(require 'helm.init)
(require 'clojure.init)
(require 'javascript.init)
(require 'typescript.init)

;; Always on to copy from/to the system clipboard.
(use-package xclip
  :config (xclip-mode 1))

(use-package graphql-mode :mode "\\.graphql")
(use-package groovy-mode :mode "\\.groovy$")
(use-package markdown-mode :mode "(\\.markdown|\\.md)$")

(use-package smart-mode-line
  :config (sml/setup))

(use-package projectile
  :diminish projectile-mode
  :init (projectile-global-mode)
  :config
  (use-package helm-projectile))

(use-package company
  :diminish company-mode
  :init (global-company-mode)
  :config
  (defun indent-or-complete ()
    (interactive)
    (if (looking-at "\\_>")
        (company-complete-common)
      (indent-according-to-mode)))
  (global-set-key "\t" 'indent-or-complete))

(use-package smartparens
  :diminish smartparens-mode
  :mode "(\\.clj|\\.cljs|\\.el)$"
  :init
  (add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
  (add-hook 'clojure-mode-hook 'smartparens-mode)
  (add-hook 'clojurescript-mode-hook 'smartparens-mode))

(use-package paren-face
  :diminish paren-face
  :mode "(\\.clj|\\.cljs|\\.el)$"
  :init (global-paren-face-mode)
  :config
  (add-hook 'clojure-mode-hook
            (lambda () (setq paren-face-regexp "#?[](){}[]"))))

(use-package neotree
  :config
  (setq neo-window-position 'right))

(use-package flycheck
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode)

  ;; Flycheck slow when opening JS files while waiting for eslint.
  ;; https://github.com/flycheck/flycheck/issues/1129
  (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t))

  (setq
   ;; Whitelist major modes with flycheck enabled.
   ;; Currently enabled for Elisp, Javascript and TypeScript.
   flycheck-global-modes '(emacs-lisp-mode js2-mode tide-mode)

   ;; Flycheck should create hidden temporary files.
   flycheck-temp-prefix ".flycheck")

  ;; Use local eslint from node_modules before global.
  ;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))

  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules))

(defun my/xml-pretty-print-buffer ()
  (interactive)
  (save-excursion
    (sgml-mode)
    (sgml-pretty-print (point-min) (point-max))
    (indent-region (point-min) (point-max))))

(require 'keybindings.init)
(provide 'init)
;;; init.el ends here
