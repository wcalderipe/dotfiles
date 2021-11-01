;;; pkg-swiper.el --- -*- lexical-binding: t; -*-

;;; Gives an overview of the current regex search candidates.

(require 'use-package)

(use-package swiper
  :straight t
  :defer t
  :init
  (setq swiper-action-recenter t)

  (general-define-key
   [remap switch-to-buffer] #'ivy-switch-buffer
   [remap list-buffers] #'ivy-switch-buffer
   [remap switch-to-buffer-other-window] #'ivy-switch-buffer-other-window
   [remap bookmark-jump] #'counsel-bookmark
   [remap describe-variable] #'counsel-describe-variable
   [remap describe-function] #'counsel-describe-function
   [remap find-file] #'counsel-find-file
   [remap execute-extended-command] #'counsel-M-x)

  (general-define-key
   :prefix my/leader
   :states 'normal
   :keymaps 'override
   "/" #'swiper))

(provide 'pkg-swiper)

;;; pkg-swiper.el ends here
