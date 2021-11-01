;;; pkg-evil.el --- -*- lexical-binding: t; -*-

;;; Vim key bindings emulator for Emacs.

(require 'use-package)

(use-package evil
  :straight t
  :preface
  ;; Do not load evil keybindings, because we'll use
  ;; from the evil-collection package.
  (setq evil-want-keybinding nil)

  ;; Allow the cursor to move beyond the last character of a line. It fixes the
  ;; annoying incompatibility of lisp modes eval last sexp.
  (setq evil-move-cursor-back t)
  (setq evil-move-beyond-eol t)

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
   :keymaps 'evil-normal-state-map
   "C-]" #'evil-goto-definition
   ;; Remove bindings conflicting with default Emacs behavior.
   "M-." nil
   "C-p" nil
   "C-n" nil)

  (evil-mode 1))

(provide 'pkg-evil)

;;; pkg-evil.el ends here
