;;; pkg-counsel.el --- -*- lexical-binding: t; -*-

(require 'use-package)

;;; Various completion functions.

(use-package counsel
  :straight t
  :defer t
  :init
  (setq counsel-describe-function-function #'helpful-callable
        counsel-describe-variable-function #'helpful-variable)

  ;; Remove "^" prefixes of my searches.
  (setq ivy-initial-inputs-alist nil)

  ;; Use custom configurations, but most importantly, pipe the filtering
  ;; from the "fdfind" output. See the `dotfiles/shell/vars' file for
  ;; more details.
  (setq counsel-fzf-cmd
        (concat (getenv "FZF_CTRL_T_COMMAND") " | " "fzf -f \"%s\""))

  (general-define-key
   [remap bookmark-jump] #'counsel-bookmark
   [remap describe-variable] #'counsel-describe-variable
   [remap describe-function] #'counsel-describe-function
   [remap find-file] #'counsel-find-file
   [remap org-set-tags-command] #'counsel-org-tag
   [remap execute-extended-command] #'counsel-M-x)

  (general-define-key
   :prefix my/leader
   :states 'normal
   :keymaps 'override
   "f" #'counsel-projectile-find-file))

(provide 'pkg-counsel)

;;; pkg-counsel.el ends here
