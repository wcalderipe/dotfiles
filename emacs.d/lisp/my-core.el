(defun my/install-package-manager ()
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  ;; Auto-install `use-package'.
  (straight-use-package 'use-package))

;; More convenient key definitions.
(use-package general
  :straight t
  :demand t
  :config
  (general-define-key
   ;; Kill the current buffer by default.
   "C-x k" #'kill-this-buffer))

(use-package org
  :straight t

  :commands (org-mode)

  :config
  ;; Record a timestamp when a todo item is DONE.
  (setq org-log-done 'time))

;; Supplemental evil-mode key-bindings to org-mode.
(use-package evil-org
  :straight t

  :after (org
          evil)

  :commands (evil-org
             evil-org-agenda)

  :init
  (add-hook 'org-mode-hook 'evil-org-mode))

;; Let's be honest here, there's nothing more productive than vi
;; key-bindings in the right hands.
(use-package evil
  :straight t

  :preface
  ;; Do not load evil keybindings, because we'll use
  ;; from the evil-collection package.
  (setq evil-want-keybinding nil)

  :init
  (defun my/evil-vim-split ()
    "Splits the current window horizontally and switch to the new window."
    (interactive)
    (evil-window-split)
    (evil-window-down 1))

  (defun my/evil-vim-vsplit ()
    "Splits the current window vertically and switch to the new window."
    (interactive)
    (evil-window-vsplit)
    (evil-window-right 1))

  (defun my/evil-enable-visual-line-navigation ()
    "Simulate evil navigation in `visual-line-mode'."
    (define-key evil-motion-state-map "0" #'evil-beginning-of-visual-line)
    (define-key evil-motion-state-map "$" #'evil-end-of-visual-line)
    (define-key evil-motion-state-map "j" #'evil-next-visual-line)
    (define-key evil-motion-state-map "k" #'evil-previous-visual-line))

  ;; Allows jumping back and forth between special buffers too.
  (setq evil--jumps-buffer-targets "\\*")

  ;; Always start in the normal mode. This is required, for example, to not enter
  ;; the git commit mode in insert mode. More often than not I have to navigate
  ;; across the diff before knowing what to write in the commit message.
  (add-hook 'with-editor-mode-hook #'evil-normal-state)

  ;; With visual-line-mode enabled it's better to navigate by visual line.
  (add-hook 'visual-line-mode-hook #'my/evil-enable-visual-line-navigation)

  ;; Always center current line while searching.
  (defadvice evil-search-next
      (after advice-for-evil-search-next activate)
    (evil-scroll-line-to-center (line-number-at-pos)))
  (defadvice evil-search-previous
      (after advice-for-evil-search-previous activate)
    (evil-scroll-line-to-center (line-number-at-pos)))

  :config
  ;; Split like Vim, i.e. moves to the newly created window.
  (evil-ex-define-cmd "split"  #'my/evil-vim-split)
  (evil-ex-define-cmd "vsplit" #'my/evil-vim-vsplit)

  (general-define-key
   :keymaps 'evil-motion-state-map
   [remap evil-beginning-of-line] #'my/smart-beginning-of-line)

  (general-define-key
   :keymaps 'evil-normal-state-map
   "C-]" #'evil-goto-definition
   ;; Remove bindings conflicting with default Emacs behavior.
   "M-." nil
   "C-p" nil
   "C-n" nil)

  (evil-mode 1))

(provide 'my-core)
