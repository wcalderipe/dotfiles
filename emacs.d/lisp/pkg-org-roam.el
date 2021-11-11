;;; pkg-org-roam.el --- -*- lexical-binding: t; -*-

;;; Effortless non-hierarchical note-taking with Org-mode.

(require 'use-package)

(use-package org-roam
  :straight t
  :ensure t
  :hook (after-init . org-roam-mode)
  :init
  ;; Custom capture template with my `setup.org' adapted from
  ;; https://www.orgroam.com/manual/Template-Walkthrough.html#Template-Walkthrough
  (defun my/org-roam-capture-templates ()
    "Create the default org-roam capture template with a custom head."
    '(("d" "Default" plain #'org-roam-capture--get-point
       "%?"
       :file-name "%<%Y%m%d%H%M%S>-${slug}"
       :unnarrowed t
       :head "#+setupfile: ~/dev/dotfiles/setup.org
#+title: ${title}\n
* ${title}\n\n")

      ("r" "Reading list" plain #'org-roam-capture--get-point
       "%?"
       :file-name "%<%Y%m%d%H%M%S>-${slug}"
       :head "#+setupfile: ~/dev/dotfiles/setup.org
#+title: ${title}\n
- tags :: [[file:reading-list.org][Reading list]]\n
* ${title}
:PROPERTIES:
:CREATED_AT: %t
:URL: nil
:STATUS: unread
:RATE: 0
:END:\n")

      ("p" "People" plain #'org-roam-capture--get-point
       "%?"
       :file-name "%<%Y%m%d%H%M%S>-${slug}"
       :head "#+setupfile: ~/dev/dotfiles/setup.org
#+title: ${title}\n
- tags :: [[file:20210429092104-people.org][People]]\n")

      ("w" "Work" plain #'org-roam-capture--get-point
       "%?"
       :file-name "%<%Y%m%d%H%M%S>-${slug}"
       :head "#+setupfile: ~/dev/dotfiles/setup.org
#+title: ${title}\n
- tags :: [[file:20210311170624-multis.org][Multis]]\n
* ${title}\n\n")))

  (defun my/org-roam-dailies-capture-templates ()
    '(("d" "Default" entry
       #'org-roam-capture--get-point
       "* %?"
       :file-name "daily/%<%Y-%m-%d>"
       ;; Format the title with the date and the day of the week.
       ;; See https://www.gnu.org/software/emacs/manual/html_node/elisp/Time-Parsing.html
       :head "#+title: %<%Y-%m-%d (%A)>\n
* Today\n
** What would make my day great?\n")))

  (setq org-roam-directory my/org-dir
        org-roam-db-location (concat my/org-dir ".config/org-roam.db")
        org-roam-capture-templates (my/org-roam-capture-templates)
        org-roam-dailies-capture-templates (my/org-roam-dailies-capture-templates))

  :config
  (general-define-key
   :keymaps 'org-roam-mode-map
   "C-c n l" #'org-roam
   "C-c n f" #'org-roam-find-file
   "C-c n g" #'org-roam-graph-show
   "C-c n t" #'org-roam-dailies-today
   "C-c n T" #'org-roam-dailies-tomorrow
   "C-c n y" #'org-roam-dailies-yesterday
   "C-c n d" #'org-roam-dailies-find-date)

  (general-define-key
   :keymaps 'org-mode-map
   "C-c n i" #'org-roam-insert
   "C-c n I" #'org-roam-insert-immediate))

(provide 'pkg-org-roam)

;;; pkg-org-roam.el ends here
