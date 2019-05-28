;; This is mandatory in order to automatically enable the git commit mode when
;; commiting from the command line.
;; For example when running:
;;   $ EDITOR=emacs git commit --verbose
(require 'git-commit)

;; We must require magit manually. Without it we'd need to run a
;; magit function (.e.g magit-status) before being able to use certain
;; keybindings, like C-x g.
(require 'magit)

;; Enable spell check automatically.
(add-hook 'git-commit-mode-hook (lambda () (flyspell-mode t)))
