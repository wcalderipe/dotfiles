;; For security reasons, copy only the absolute minimum environment
;; variables to Emacs GUI.
(when (display-graphic-p)
  (setq exec-path-from-shell-variables
        '("FZF_DEFAULT_COMMAND"
          "FZF_DEFAULT_OPTS"
          "PATH"))
  (exec-path-from-shell-initialize))
