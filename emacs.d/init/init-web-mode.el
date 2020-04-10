;; JavaScript

;; Force js2-mode over js-mode
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Set rjsx-mode if React is present in the file.
;; The detect and enable code was inspired by the Emacs 27 which is
;; still under development.
;; See https://github.com/emacs-mirror/emacs/blob/ee89c1cdb5a3bb7b69b763a59a20b34508ddf3ae/lisp/progmodes/js.el#L4485-L4525
(add-hook 'js2-mode-hook (lambda ()
                           (when (and (not (eq major-mode 'rjsx-mode))
                                   (re-search-forward "\\_<\\(?:var\\|let\\|const\\|import\\)\\_>.*?React" 4000 t))
                             (rjsx-mode))))

;; Remove pretiffy arrow functions and family.
(setq js--prettify-symbols-alist nil)

;; TypeScript

;; Highlight instances of the identifier at point after a short
;; timeout.
(tide-hl-identifier-mode 1)

;; Spin tide server
(add-hook 'typescript-mode-hook 'tide-setup)

;; Start the tide serve for `.tsx` files.
(add-hook 'js2-mode-hook (lambda ()
                           (when (string-equal "tsx" (file-name-extension buffer-file-name))
                             (tide-setup))))

(add-hook 'typescript-mode-hook 'flycheck-mode)

(flycheck-add-mode 'typescript-tslint 'typescript-mode)
