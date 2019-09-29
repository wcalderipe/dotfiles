;; Avoid using fuzzy searches everywhere. For example, counsel-rg
;; with fuzzy enabled brings a lot of useless results.
;; Remember you can switch modes in the ivy minibuffer with <C-o S-m>.
(setq ivy-re-builders-alist '((counsel-M-x . ivy--regex-fuzzy)
                              (t . ivy--regex-plus)))

;; Do not display the total number of candidates.
(setq ivy-count-format "")

;; Hide directories ./ and ../
(setq ivy-extra-directories nil)

;; Enable bookmarks and recentf.
(setq ivy-use-virtual-buffers t)

;; Disable annoying configuration which uses the character "^" in
;; regex searches.
(setq ivy-initial-inputs-alist nil)

;; Do not close the minibuffer when there's no text left to delete.
(setq ivy-on-del-error-function nil)

;; Use only one foreground color for all match groups, just like VS
;; Code and many other editors.
;; `nil` means it will use the same color as the inherited face.
;; `unspecified` is used to reset any style set by the parent face.
(let* ((foreground-color "#93E0E3")
       (highlight-color "gray40"))
  (custom-set-faces
   `(ivy-current-match              ((t :background ,highlight-color :foreground nil :underline unspecified :weight unspecified)))
   `(ivy-highlight-face             ((t :background nil :foreground nil :underline unspecified :weight unspecified)))
   `(ivy-minibuffer-match-face-1    ((t :background nil :inherit bold)))
   `(ivy-minibuffer-match-face-2    ((t :background nil :foreground ,foreground-color :underline t)))
   `(ivy-minibuffer-match-face-3    ((t :background nil :foreground ,foreground-color :underline t)))
   `(ivy-minibuffer-match-face-4    ((t :background nil :foreground ,foreground-color :underline t)))
   `(ivy-minibuffer-match-highlight ((t :background ,highlight-color :foreground nil :underline unspecified :weight unspecified)))
   `(ivy-subdir                     ((t :background nil :underline unspecified :weight unspecified)))))

(ivy-mode 1)
