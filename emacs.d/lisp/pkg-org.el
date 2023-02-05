;;; pkg-org.el --- -*- lexical-binding: t; -*-

(require 'use-package)

(use-package org
  :straight t
  :commands (org-mode)
  :hook (org-mode . flyspell-mode)
  :preface
  (setq org-directory my/org-dir
        ;; Stores notes in the Org directory.
        org-default-notes-file (concat my/org-dir "/notes.org")
        ;; Sets agenda file
        org-agenda-files (list my/org-dir
                               (concat my/org-dir "/daily"))
        ;; Stores ID locations in the cache directory.
        org-id-locations-file (concat my/cache-dir "org-id-locations"))

  ;; Removes footnote HTML validation link.
  (setq org-html-validation-link nil)

  ;; Opens org files with all headlines visible.
  (setq org-startup-folded 'showall)

  ;; Records a timestamp when a todo item is DONE.
  (setq org-log-done 'time)

  ;; Place tags directly after headline text, with only one space in between.
  (setq org-tags-column 0)

  (setq org-todo-keywords
        '((sequence
           "TODO"
           "DONE")))

  ;; Expand priority from A to D so it can match with Eisenhower's matrix
  ;; quadrants.
  (setq org-highest-priority ?A
        org-default-priority ?B
        org-lowest-priority  ?D)

  :config
  ;; Disables auto indentation in BEGIN blocks. Let me handle it.
  (add-hook 'org-mode-hook (lambda () (electric-indent-mode -1)))

  ;;;; Babel

  (setq org-confirm-babel-evaluate nil)

  ;; Languages which can be evaluated in Org buffers.
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((plantuml . t)
     (shell . t)
     (clojure . t)))

;;;; Keybinding

  (general-define-key
   :prefix my/mode-leader
   :states 'normal
   :keymaps 'org-mode-map
   "S"   #'org-schedule
   "p"   #'org-priority
   "s"   #'org-sort
   "t c" #'org-todo
   "t t" #'org-show-todo-tree))

(provide 'pkg-org)

;;; pkg-org.el ends here
