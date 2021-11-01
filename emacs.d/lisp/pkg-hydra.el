;;; pkg-hydra.el --- -*- lexical-binding: t; -*-

(require 'use-package)

(use-package hydra
  :straight t
  :defer t
  :init
  (defun pkg-hydra/counsel-projectile-switch-project-action-dired (project)
    "Open dired when switching projects with counsel-projectile."
    (let ((projectile-switch-project-action
           (lambda ()
             (projectile-dired))))
      (counsel-projectile-switch-project-by-name project)))

  (defun pkg-hydra/counsel-projectile-switch-project-dotfiles ()
    "Open my dotfiles project straightaway."
    (interactive)
    (pkg-hydra/counsel-projectile-switch-project-action-dired "~/dev/dotfiles"))

  (defun pkg-hydra/dired-dotfiles-toggle ()
    "Show/hide dotfiles"
    (interactive)
    (when (equal major-mode 'dired-mode)
      ;; If currently showing
      (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p)
          (progn
            (set (make-local-variable 'dired-dotfiles-show-p) nil)
            (dired-mark-files-regexp "^\\\.")
            (dired-do-kill-lines))
        ;; Otherwise just revert to re-show
        (progn (revert-buffer)
               (set (make-local-variable 'dired-dotfiles-show-p) t)))))

  (defun pkg-hydra/async-shell-command-no-window (command)
    "Execute string COMMAND asynchronously without opening buffer."
    (interactive "sAsync shell command: ")
    (let* ((buffer-name "*Async Shell Command*")
           (output-buffer (get-buffer-create buffer-name))
           (process (let ((display-buffer-alist (list (list buffer-name #'display-buffer-no-window))))
                      (async-shell-command command output-buffer)
                      (get-buffer-process output-buffer)))
           (sentinel `(lambda (process signal)
                        (when (memq (process-status process) '(exit signal))
                          (shell-command-sentinel process signal)
                          ;; Here you could run arbitrary code when the
                          ;; command is successful.
                          ;; (when (zerop (process-exit-status process))
                          ;;   (message "%s" ,cmd))
                          ))))
      (when (process-live-p process)
        (set-process-sentinel process sentinel))))

  (defun pkg-hydra/async-shell-region-no-window (begin end)
    "Execute the REGION as a COMMAND asynchronously without opening buffer."
    (interactive "r")
    (pkg-hydra/async-shell-command-no-window
     (buffer-substring-no-properties begin end)))

  (defun pkg-hydra/projectile-run-async-shell-command-no-window-in-root ()
    "Invoke `pkg-hydra/async-shell-command-no-window' in the project's root."
    (interactive)
    (projectile-with-default-dir (projectile-ensure-project (projectile-project-root))
      (call-interactively 'pkg-hydra/async-shell-command-no-window)))

  (defun pkg-hydra/window-resize-right (arg)
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (enlarge-window-horizontally arg)
      (shrink-window-horizontally arg)))

  (defun pkg-hydra/window-resize-left (arg)
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (shrink-window-horizontally arg)
      (enlarge-window-horizontally arg)))

  (defun pkg-hydra/window-resize-up (arg)
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (enlarge-window arg)
      (shrink-window arg)))

  (defun pkg-hydra/window-resize-down (arg)
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (shrink-window arg)
      (enlarge-window arg)))

  :config
  ;; For more hydra examples, have a look at:
  ;; https://github.com/abo-abo/hydra/blob/master/hydra-examples.el

  (defhydra hydra-projectile (:hint nil :foreign-keys nil :exit t :idle 0.5)
    "
PROJECT: %(projectile-project-root)

 ^Search^                ^Buffers^                    ^Cache^                 ^Command
^^^^^^^----------------------------------------------------------------------------------------------
 _s_: rg (mini-buffer)   _a_: alternate               _c_: cache clear        _r_ run async
 _S_: rg (deadgrep)      _A_: alternate (other win)   _i_: include project    _R_ run async no window
 ^ ^                     _b_: switch to buffer        _x_: remove project     _C_ compile
 ^ ^                     _d_: dired                   _X_: cleanup projects   _T_ test
 ^ ^                     _K_: kill all buffers        _z_: cache current

"
    ("A" projectile-find-implementation-or-test-other-window)
    ("C" projectile-compile-project)
    ("K" projectile-kill-buffers)
    ("R" pkg-hydra/projectile-run-async-shell-command-no-window-in-root)
    ("S" deadgrep)
    ("T" projectile-test-project)
    ("X" projectile-cleanup-known-projects)
    ("a" projectile-toggle-between-implementation-and-test)
    ("b" counsel-projectile-switch-to-buffer)
    ("c" projectile-invalidate-cache)
    ("d" projectile-dired)
    ("i" projectile-add-known-project)
    ("r" projectile-run-async-shell-command-in-root)
    ("s" counsel-projectile-rg)
    ("x" projectile-remove-known-project)
    ("z" projectile-cache-current-file)

    ("p" counsel-projectile-switch-project "switch project")
    ("." pkg-hydra/counsel-projectile-switch-project-dotfiles "switch dotfiles")
    ("q" nil "quit"))

  (defhydra hydra-dired (:hint nil :foreign-keys run :exit nil)
    "
_v_: view         _m_: mark           _l_: redisplay       _i_: insert subdir   wdired
_V_: view other   _u_: unmark         _g_: refresh         _$_: hide subdir     C-x C-q: edit
_o_: open other   _U_: unmark all     _=_: diff            _w_: kill subdir     C-c C-c: commit
_M_: chmod        _t_: toggle marks   _s_: sort            _X_: shell command   C-c ESC: abort
_G_: chgrp        _S_: symlink        _H_: toggle hidden
_O_: chown        _Z_: zip/unzip
^ ^
"
    ("$" diredp-hide-subdir-nomove)
    ("=" diredp-ediff)
    ("G" dired-do-chgrp)
    ("H" pkg-dired/dired-dotfiles-toggle)
    ("M" dired-do-chmod)
    ("O" dired-do-chown)
    ("S" dired-do-symlink)
    ("T" dired-hide-details-mode)
    ("U" dired-unmark-all-marks)
    ("V" dired-display-file)
    ("X" dired-do-shell-command)
    ("Z" dired-do-compress)
    ("e" dired-ediff-files)
    ("g" revert-buffer)
    ("i" dired-maybe-insert-subdir)
    ("l" dired-do-redisplay)
    ("m" dired-mark)
    ("o" dired-find-file-other-window)
    ("s" dired-sort-toggle-or-edit)
    ("t" dired-toggle-marks)
    ("u" dired-unmark)
    ("v" dired-view-file)
    ("w" dired-kill-subdir)

    ("C" dired-do-copy          "copy")
    ("D" dired-do-delete        "remove")
    ("+" dired-create-directory "mkdir")
    ("R" dired-do-rename        "rename")
    ("q" nil                    "quit"))

  (defhydra hydra-window (:hint nil :exit nil)
    ("=" balance-windows "balance windows")
    ("h" buf-move-left   "swap left")
    ("j" buf-move-down   "swap down")
    ("k" buf-move-up     "swap up")
    ("l" buf-move-right  "swap right")
    ("u" winner-undo     "undo")
    ("r" winner-redo     "redo")
    ("q" nil             "quit"))

  (defhydra hydra-tab (:hint nil :exit nil)
    ("+" eyebrowse-create-window-config "create")
    ("-" eyebrowse-close-window-config  "remove")
    ("l" eyebrowse-next-window-config   "next")
    ("h" eyebrowse-prev-window-config   "previous")
    ("q" nil                            "quit"))

  (general-define-key
   :prefix "C-c"
   "w" #'hydra-window/body
   "p" #'hydra-projectile/body
   "t" #'hydra-tab/body)

  (general-define-key
   :keymaps 'dired-mode-map
   "C-c d" #'hydra-dired/body))

(provide 'pkg-hydra)

;;; pkg-hydra.el ends here
