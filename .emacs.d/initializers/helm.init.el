(use-package helm
  :diminish helm-mode
  :init (progn
          (require 'helm-config)

          ;; Enable helm fuzzy match
          (setq helm-mode-fuzzy-match t
                helm-completion-in-region-fuzzy-match t
                helm-candidate-number-limit 100
                helm-buffers-fuzzy-matching t
                helm-recentf-fuzzy-match t
                helm-locate-fuzzy-match t
                helm-M-x-fuzzy-match t
                helm-apropos-fuzzy-match t
                helm-projectile-fuzzy-match t
                helm-lisp-fuzzy-completion t)

          ;; From https://gist.github.com/antifuchs/9238468
          (setq  helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
                 helm-input-idle-delay 0.01  ; this actually updates things ; reeeelatively quickly.
                 helm-yas-display-key-on-candidate t

                 helm-quick-update t
                 helm-M-x-requires-pattern nil
                 helm-ff-skip-boring-files t)

          (helm-mode))

  :bind (("C-c h"   . helm-mini)
         ("C-x C-b" . helm-buffers-list)
         ("C-x b"   . helm-buffers-list)
         ("M-y"     . helm-show-kill-ring)
         ("M-x"     . helm-M-x)
         ("C-x c o" . helm-occur)
         ("C-p"     . helm-projectile-find-file)))

(use-package helm-ag)

(ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally

(provide 'helm.init)
;;; helm.init.el ends here
