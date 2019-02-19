(use-package evil
  :diminish evil-mode
  :init (progn
          ;; Enable C-u just like Vim
          (setq evil-want-C-u-scroll t)

          ;; Allows jumping back and forth between special buffers too.
          (setq evil--jumps-buffer-targets "\\*")

          (evil-mode 1)

          (defun my/scroll-line-to-center ()
            (evil-scroll-line-to-center (line-number-at-pos)))

          (defun my/jump-to-source ()
            "Jump to source as if using ctags (only clojure-mode) and scroll to
            center automatically."
            (interactive)
            (cond ;; Clojure mode.
                  ((eq major-mode 'clojure-mode)
                   (progn
                     (cider-find-var)
                     (my/scroll-line-to-center)))

                  ;; Javascript mode. Requires xref-js2 package to work.
                  ((eq major-mode 'js2-mode)
                   (progn
                     (xref-find-definitions (symbol-name (symbol-at-point)))
                     (my/scroll-line-to-center)))

                  ;; TypeScript mode.
                  ((eq major-mode 'typescript-mode)
                   (tide-jump-to-definition))

                  ;; Default jump to source strategy.
                  (t (evil-jump-to-tag))))

          ;; Remove some evil bindings.
          (define-key evil-normal-state-map (kbd "M-.") nil)
          (define-key evil-normal-state-map (kbd "C-p") nil)

          (defun my/vim-split ()
            "Splits the current window horizontally and switch to the new
            window."
            (interactive)
            (evil-window-split)
            (evil-window-down 1))

          (defun my/vim-vsplit ()
            "Splits the current window vertically and switch to the new
            window."
            (interactive)
            (evil-window-vsplit)
            (evil-window-right 1))

          ;; Split like Vim, i.e. moves to the newly created window.
          (evil-ex-define-cmd "split"  'my/vim-split)
          (evil-ex-define-cmd "vsplit" 'my/vim-vsplit)

          ;; Replace default evil-jump-to-tag command.
          (define-key evil-normal-state-map (kbd "C-]") #'my/jump-to-source)

          ;; Add to jump list, i.e. record location prior to running commands.
          (evil-add-command-properties #'my/jump-to-source :jump t)
          (evil-add-command-properties #'cider-test-run-project-tests :jump t)

          ;; Always center current line while searching.
          (defadvice evil-search-next
              (after advice-for-evil-search-next activate)
            (evil-scroll-line-to-center (line-number-at-pos)))
          (defadvice evil-search-previous
              (after advice-for-evil-search-previous activate)
            (evil-scroll-line-to-center (line-number-at-pos)))

          (use-package evil-cleverparens
            :diminish evil-cleverparens-mode
            :init (add-hook 'smartparens-mode-hook 'evil-cleverparens-mode))

          (use-package evil-surround
            :diminish evil-surround-mode
            :init (global-evil-surround-mode 1)
            :config
            (add-to-list 'evil-surround-operator-alist '(evil-cp-change . change))
            (add-to-list 'evil-surround-operator-alist '(evil-cp-delete . delete)))

          (use-package evil-leader
            :diminish evil-leader-mode
            :init
            (setq evil-leader/in-all-states 1) ;; Fix issue where leader may not work on some modes
            (global-evil-leader-mode))))

(provide 'evil.init)
;;; evil.init.el ends here
