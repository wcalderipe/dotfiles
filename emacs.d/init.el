(add-to-list 'load-path (concat user-emacs-directory "lisp/"))

(require 'my-vars)
(require 'my-lib)
(require 'my-emacs)

(my/install-package-manager)
(require 'my-packages-core)
(require 'my-packages-languages)
(require 'my-packages-misc)
(require 'my-packages-gui)
(require 'my-editor)
