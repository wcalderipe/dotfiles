;; Displays available keybindings (discoverability).
(use-package which-key
  :straight t

  :defer t

  :config
  (which-key-setup-minibuffer)
  (which-key-mode))


;; More friendly interface for ivy (discoverability).
(use-package ivy-rich
  :straight t

  :defer t

  :config
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
	     (:face font-lock-doc-face))))))

  :init
  (ivy-rich-mode))


;; Helpful package to quick share fragments of code with your
;; teammates.
(use-package git-link
  :straight t

  :defer t

  :config
  ;; This might create a reference in the wrong lines if you're using
  ;; branches for development and your current version is too far
  ;; ahead from origin.
  (setq git-link-default-branch "master"))

(provide 'my-packages-misc)
