(require 'counsel-projectile)

;; Open dired at the root of the project instead of asking me which
;; file I want to open, because often when I switch to a project I don't
;; know upfront exactly which file I want to open. Therefore, this is a
;; more sensible default."
(counsel-projectile-modify-action
  'counsel-projectile-switch-project-action
  '((add ("." my/counsel-projectile-switch-project-action-dired)
         1)))
