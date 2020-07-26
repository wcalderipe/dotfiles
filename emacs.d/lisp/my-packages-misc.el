;;
;; Discoverability.
;;

;; Displays available keybindings.
(use-package which-key
  :straight t

  :defer t

  :config
  (which-key-setup-minibuffer)
  (which-key-mode))


;; More friendly interface for ivy.
(use-package ivy-rich
  :straight t

  :defer t

  :init
  (ivy-rich-mode))

(provide 'my-packages-misc)
