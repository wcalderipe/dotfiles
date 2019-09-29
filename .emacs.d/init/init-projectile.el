(setq projectile-completion-system 'ivy)

;; It's recommended to use fd as a replacement for both git ls-files and find.
;; both git ls-files (fd understands .gitignore) and find.
(setq projectile-generic-command "fd . -0")

;; Disable caching
(setq projectile-enable-caching nil)

;; Disable Projectile mode line due to performance issues.
(setq projectile-mode-line nil)

;; Enable Projectile in every directory (even without the presence
;; of project file). This works well with fd, given how much faster
;; it is compared to find.
(setq projectile-require-project-root nil)

;; Skip warnings about unsafe variables in .dir-locals.el
(put 'projectile-project-type 'safe-local-variable #'symbolp)

(projectile-global-mode)
