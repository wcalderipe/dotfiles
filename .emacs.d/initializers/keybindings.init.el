(evil-leader/set-leader "SPC")

(evil-leader/set-key
  "rd" 'cider-doc
  "re" 'cider-eval-defun-at-point
  "rE" 'cider-eval-last-sexp
  "rf" 'my/format-and-clean-buffer
  "rp" 'cider-pprint-eval-defun-at-point
  "rP" 'cider-pprint-eval-last-sexp
  "rr" 'cider-refresh
  "rs" 'cider-browse-spec
  "ta" 'cider-test-run-project-tests
  "tf" 'cider-test-run-ns-tests
  "tl" 'cider-test-rerun-test
  "tt" 'cider-test-run-test)

(evil-leader/set-key
  "/"  'helm-occur
  "uu" 'helm-mini             ; b<u>ffer list
  "fp" 'helm-ag-project-root) ; <f>ind in <p>roject

(evil-leader/set-key
  "wu" 'winner-undo
  "wr" 'winner-redo)

(evil-leader/set-key
  "ja" 'projectile-toggle-between-implementation-and-test
  "jA" 'projectile-find-implementation-or-test-other-window)

(evil-leader/set-key "nt" 'neotree-toggle)
(evil-define-key
  'normal neotree-mode-map
  (kbd "go")  'neotree-quick-look
  (kbd "i")   'neotree-enter-horizontal-split
  (kbd "I")   'neotree-hidden-file-toggle ; Same as NERDTree
  (kbd "ma")  'neotree-create-node ; Modify -> create
  (kbd "md")  'neotree-delete-node ; Modify -> delete
  (kbd "mr")  'neotree-rename-node ; Modify -> rename
  (kbd "o")   'neotree-enter ; Open (same as NERDTree)
  (kbd "q")   'neotree-hide
  (kbd "R")   'neotree-refresh
  (kbd "s")   'neotree-enter-vertical-split
  (kbd "RET") 'neotree-enter)

(provide 'keybindings.init)
;;; keybindings.init.el ends here
