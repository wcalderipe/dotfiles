(add-to-list 'load-path (concat user-emacs-directory "lisp/"))

(require 'my-vars)
(require 'my-lib)
(require 'my-emacs)

(my/install-package-manager)

;;; Core
(require 'pkg-general)
(require 'pkg-restart-emacs)
(require 'pkg-company)
(require 'pkg-evil)
(require 'pkg-evil-collection)
(require 'pkg-evil-surround)
(require 'pkg-evil-nerd-commenter)
(require 'pkg-dired)
(require 'pkg-magit)
(require 'pkg-xclip)
(require 'pkg-counsel)
(require 'pkg-swiper)
(require 'pkg-projectile)
(require 'pkg-counsel-projectile)
(require 'pkg-buffer-move)
(require 'pkg-eyebrowse)
(require 'pkg-ivy)
(require 'pkg-ivy-rich)
(require 'pkg-amx)
(require 'pkg-deadgrep)
(require 'pkg-hydra)
(require 'pkg-default-text-scale)
(require 'pkg-exec-path-from-shell)

;;; Theme
(require 'pkg-zenburn-theme)

;;; UI
(require 'pkg-all-the-icons)
(require 'pkg-all-the-icons-ivy)
(require 'pkg-all-the-icons-ivy-rich)
(require 'pkg-all-the-icons-dired)
(require 'pkg-all-the-icons-dired)
(require 'pkg-doom-modeline)

;;; Miscellaneous
(require 'pkg-which-key)
(require 'pkg-flyspell)
(require 'pkg-flyspell-correct-ivy)
(require 'pkg-editorconfig)
(require 'pkg-git-link)
(require 'pkg-visual-fill-column)
(require 'pkg-recentf)
(require 'pkg-smartparens)
(require 'pkg-paredit)
(require 'pkg-agressive-indent)
(require 'pkg-helpful)
(require 'pkg-highlight-symbol)
(require 'pkg-outline)

;;; Flycheck
(require 'pkg-flycheck)
(require 'pkg-flycheck-clj-kondo)

;;; Org
(require 'pkg-org)
(require 'pkg-org-evil)
(require 'pkg-org-roam)
(require 'pkg-org-cliplink)
(require 'pkg-org-ql)

;;; LSP
(require 'pkg-lsp-mode)
(require 'pkg-lsp-ivy)

;;; Languages
(require 'pkg-emacs-lisp-mode)
(require 'pkg-cider)
(require 'pkg-cider-hydra)
(require 'pkg-clojure-mode)
(require 'pkg-dockerfile-mode)
(require 'pkg-csv-mode)
(require 'pkg-json-mode)
(require 'pkg-graphql-mode)
(require 'pkg-solidity-mode)
(require 'pkg-markdown-mode)
(require 'pkg-plantuml-mode)
(require 'pkg-web-mode)
(require 'pkg-typescript-mode)
