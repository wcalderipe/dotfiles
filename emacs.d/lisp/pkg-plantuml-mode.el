;;; pkg-plantuml-mode.el --- -*- lexical-binding: t; -*-

(require 'use-package)

(use-package plantuml-mode
  :straight t
  :defer t
  :mode ("\\.puml\\'" . plantuml-mode)
  :init
  (setq plantuml-executable-path "~/.local/bin/plantuml")
  (setq plantuml-default-exec-mode 'executable)

  ;; `org-babel' uses the jar path instead of the executable.
  (setq org-plantuml-jar-path "/opt/homebrew/Cellar/plantuml/1.2021.2/libexec/plantuml.jar"))

(provide 'pkg-plantuml-mode)

;;; pkg-plantuml-mode.el ends here
