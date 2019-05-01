;; Allows jumping back and forth between special buffers too.
(setq evil--jumps-buffer-targets "\\*")

;; Split like Vim, i.e. moves to the newly created window.
(evil-ex-define-cmd "split"  'my/vim-split)
(evil-ex-define-cmd "vsplit" 'my/vim-vsplit)

;; Add to jump list, i.e. record location prior to running commands.
(evil-add-command-properties #'my/jump-to-source :jump t)

;; Always center current line while searching.
(defadvice evil-search-next
  (after advice-for-evil-search-next activate)
  (evil-scroll-line-to-center (line-number-at-pos)))
(defadvice evil-search-previous
  (after advice-for-evil-search-previous activate)
  (evil-scroll-line-to-center (line-number-at-pos)))

;; Always start in the normal mode. This is required, for example, to not enter
;; the git commit mode in insert mode. More often than not I have to navigate
;; across the diff before knowing what to write in the commit message.
(add-hook 'with-editor-mode-hook 'evil-normal-state)

(evil-mode 1)
