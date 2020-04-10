(evil-leader/set-leader "SPC")

(evil-leader/set-key-for-mode 'clojure-mode
  "d"  'cider-doc
  "e"  'cider-eval-defun-at-point
  "E"  'cider-eval-last-sexp
  "f"  'my/cider-format-and-clean-buffer
  "p"  'cider-pprint-eval-defun-at-point
  "P"  'cider-pprint-eval-last-sexp
  "r"  'cider-load-all-project-ns
  "s"  'cider-browse-spec
  "ta" 'cider-test-run-project-tests
  "tf" 'cider-test-run-ns-tests
  "tl" 'cider-test-rerun-test
  "tt" 'cider-test-run-test)

(evil-leader/set-key-for-mode 'ruby-mode
  "r"  'projectile-rails-command-map
  "e"  'ruby-send-block
  "E"  'ruby-send-region
  "ta" 'rspec-verify-all
  "tf" 'rspec-verify
  "tl" 'rspec-rerun
  "tt" 'rspec-verify-single)

(evil-leader/set-key-for-mode 'rjsx-mode
  "r" 'rjsx-rename-tag-at-point)

;; Use the same eval keybindings used for the clojure-mode.
(evil-leader/set-key-for-mode 'emacs-lisp-mode
  "e" 'eval-defun
  "E" 'eval-last-sexp)

(evil-leader/set-key
  "/"  'swiper
  "jA" 'projectile-find-implementation-or-test-other-window
  "ja" 'projectile-toggle-between-implementation-and-test
  "nt" 'neotree-toggle
  "wr" 'winner-redo
  "wu" 'winner-undo)

(evil-define-key 'normal neotree-mode-map
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

(evil-define-key 'normal deadgrep-mode-map
  ;; The usual Vim bindings to jump back and forth are not set in
  ;; evil-collection-deadgrep.el
  (kbd "C-i") 'evil-jump-forward
  (kbd "C-o") 'evil-jump-backward)

;; For some reason this keybinding ceases to exist sometimes, so this
;; line defines it globally.
(global-set-key (kbd "C-x C-j") 'dired-jump)

(global-set-key (kbd "C-c C-l") 'my/reload-init-file)
(global-set-key (kbd "C-C w") 'hydra-window/body)
(define-key dired-mode-map (kbd "C-c d") 'hydra-dired/body)

;; Zoom in/out with hydra.
(when (display-graphic-p)
  (global-set-key (kbd "C-c z") 'hydra-zoom/body))

;; Counsel M-x brings enhancements, such as using the amx package to
;; display the most recently used commands first.
(global-set-key (kbd "M-x") 'counsel-M-x)

;; Use Counsel alternatives with ivy-rich to display inline help for
;; each variable/function.
(global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key (kbd "C-h f") 'counsel-describe-function)

;; Projectile and counsel-projectile keybindings are only available
;; after we select a keymap prefix and we bind to hydra.
(define-key projectile-mode-map (kbd "C-c p") 'hydra-projectile/body)

;; ===============
;; Find operations:
;; ===============
;;
;; Quickly open files like Vim's Ctrl-P, or VS Code Cmd-P (in iTerm, you can
;; remap Meta to Command).
(global-set-key (kbd "M-p") 'counsel-fzf)
(key-chord-define evil-normal-state-map "ff" 'counsel-fzf)

;; Find buffers.
(global-set-key (kbd "C-x C-b") 'ivy-switch-buffer)
(global-set-key (kbd "C-x b")   'ivy-switch-buffer)

;; Improved toggle comment/uncomment lines.
(global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)

;; -------------------------------------------------------------------------

;; Remove bindings conflicting with default Emacs behavior.
(define-key evil-normal-state-map (kbd "M-.") nil)
(define-key evil-normal-state-map (kbd "C-p") nil)
(define-key evil-normal-state-map (kbd "C-n") nil)

;; Replace default evil-jump-to-tag command.
(define-key evil-normal-state-map (kbd "C-]") #'my/jump-to-source)

;; The magit status keybinding is not enabled for all modes, but
;; pretty much all Projectile projects I work on use Git, so let's
;; enable the keybinding globally.
(global-set-key (kbd "C-x g") 'magit-status)

;; Kill the current buffer by default.
(global-set-key (kbd "C-x k") 'kill-this-buffer)
