(setq ivy-rich-path-style 'abbreviate)

;; These configurations were adapted from the README:
;; https://github.com/Yevgnen/ivy-rich
(setq ivy-rich-display-transformers-list
      '(ivy-switch-buffer
        (:columns
         ((ivy-rich-candidate
           (:width 30))
          ((lambda (candidate)
             (file-name-directory (ivy-rich-switch-buffer-path candidate)))
           (:width (lambda (x)
                     (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
         :predicate (lambda (candidate)
                      (get-buffer candidate)))
        counsel-M-x
        (:columns
         ((counsel-M-x-transformer
           (:width 40))
          (ivy-rich-counsel-function-docstring
           (:face font-lock-doc-face))))
        counsel-describe-function
        (:columns
         ((counsel-describe-function-transformer
           (:width 40))
          (ivy-rich-counsel-function-docstring
           (:face font-lock-doc-face))))
        counsel-describe-variable
        (:columns
         ((counsel-describe-variable-transformer
           (:width 40))
          (ivy-rich-counsel-variable-docstring
           (:face font-lock-doc-face))))
        counsel-recentf
        (:columns
         ((ivy-rich-candidate
           (:width 0.8))
          (ivy-rich-file-last-modified-time
           (:face font-lock-comment-face))))))

(ivy-rich-mode 1)
