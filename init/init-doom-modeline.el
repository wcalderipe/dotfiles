;; Determines the style used by `doom-modeline-buffer-file-name'.
;;
;; Given ~/Projects/FOSS/emacs/lisp/comint.el
;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
;;   truncate-with-project => emacs/l/comint.el
;;   truncate-except-project => ~/P/F/emacs/l/comint.el
;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
;;   truncate-all => ~/P/F/e/l/comint.el
;;   relative-from-project => emacs/lisp/comint.el
;;   relative-to-project => lisp/comint.el
;;   file-name => comint.el
;;   buffer-name => comint.el<2> (uniquify buffer name)
;;
(setq doom-modeline-buffer-file-name-style 'relative-from-project)

;; How tall the mode-line should be (only respected in GUI Emacs).
(setq doom-modeline-height 25)

;; How wide the mode-line bar should be (only respected in GUI Emacs).
(setq doom-modeline-bar-width 1)

;; If non-nil, the mode-line is displayed with the `variable-pitch' face.
(setq doom-modeline-enable-variable-pitch nil)

;; Whether show `all-the-icons' or not (when nil nothing will be showed).
(setq doom-modeline-icon nil)

;; Whether display minor modes or not. Non-nil to display in mode-line.
(setq doom-modeline-minor-modes nil)

;; Change the default vibrant red color because we usually associate
;; it with errors.
(let ((color "#DFAF8F"))
  (custom-set-faces
   `(doom-modeline-evil-insert-state ((t :foreground ,color :weight bold)))
   `(doom-modeline-buffer-modified   ((t :foreground ,color)))))

(doom-modeline-mode 1)
