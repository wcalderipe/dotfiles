;; For more hydra examples, have a look at:
;; https://github.com/abo-abo/hydra/blob/master/hydra-examples.el
(require 'hydra)

(defhydra hydra-projectile (:hint nil
                                  :foreign-keys nil
                                  :exit t
                                  :idle 0.5)
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
  ("R" my/projectile-run-async-shell-command-no-window-in-root)
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
  ("H" my/dired-dotfiles-toggle)
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

(defhydra hydra-zoom (:hint nil :exit nil)
  ("+" default-text-scale-increase "increase")
  ("-" default-text-scale-decrease "decrease")
  ("0" default-text-scale-reset "reset")
  ("q" nil "quit"))

(defhydra hydra-window (:hint nil :exit nil)
  "
^Switch^    ^Swap^    ^Resize^    ^Layout^
^^^^^^^^-------------------------------------
_h_ ←       _H_ ←     _a_ ←       _u_ undo
_j_ ↓       _J_ ↓     _s_ ↓       _r_ restore
_k_ ↑       _K_ ↑     _w_ ↑       _=_ balance
_l_ →       _L_ →     _d_ →
"
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)

  ("H" buf-move-left)
  ("J" buf-move-down)
  ("K" buf-move-up)
  ("L" buf-move-right)

  ("=" balance-windows)
  ("u" winner-undo)
  ("r" winner-redo)

  ("a" (lambda (arg)
         (interactive "p")
         (if (let ((windmove-wrap-around))
               (windmove-find-other-window 'right))
           (shrink-window-horizontally arg)
           (enlarge-window-horizontally arg))))

  ("s" (lambda (arg)
         (interactive "p")
         (if (let ((windmove-wrap-around))
               (windmove-find-other-window 'up))
           (shrink-window arg)
           (enlarge-window arg))))

  ("w" (lambda (arg)
         (interactive "p")
         (if (let ((windmove-wrap-around))
               (windmove-find-other-window 'up))
           (enlarge-window arg)
           (shrink-window arg))))

  ("d" (lambda (arg)
         (interactive "p")
         (if (let ((windmove-wrap-around))
               (windmove-find-other-window 'right))
           (enlarge-window-horizontally arg)
           (shrink-window-horizontally arg)))))
