;; The following customizations try to mimic Vim tabs, or at least my
;; basic usage on a daily basis.
;; https://vim.fandom.com/wiki/Using_tab_pages

;; Use the scratch buffer when creating new tabs.
(setq eyebrowse-new-workspace t)

;; Cycle through tabs.
(setq eyebrowse-wrap-around t)

;; Define a tabedit command (a la Vim) to create new tabs with
;; optional file name or directory name. When no filename is passed on
;; it calls the default eyebrowse function.
(evil-define-command my/eyebrowse-create-window-config-with-file (file)
  :repeat nil
  (interactive "<f>")
  (if (and file (f-exists? file))
    (progn
      (eyebrowse-create-window-config)
      (find-file file))
    (eyebrowse-create-window-config)))

(evil-ex-define-cmd "tabc[lose]" 'eyebrowse-close-window-config)
(evil-ex-define-cmd "tabe[dit]"  'my/eyebrowse-create-window-config-with-file)
(evil-ex-define-cmd "tabfirst"   'eyebrowse-switch-to-window-config-0)
(evil-ex-define-cmd "tablast"    'eyebrowse-last-window-config)
(evil-ex-define-cmd "tabn"       'eyebrowse-next-window-config)
(evil-ex-define-cmd "tabp"       'eyebrowse-prev-window-config)
(evil-ex-define-cmd "tabs"       'eyebrowse-switch-to-window-config)

;; This one doesn't exist in Vim, but it's useful if you'd like to use
;; tabs like Tmux, where it's very common to rename tabs.
(evil-ex-define-cmd "tabr[ename]" 'eyebrowse-rename-window-config)

(eyebrowse-setup-evil-keys)
(eyebrowse-mode t)
