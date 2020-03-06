(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))

;; Turn on projectile-rails for buffers which belong to Rails either
;; an application or engine.
(projectile-rails-global-mode)

;; Give projectile-rails a bind.
(define-key projectile-rails-mode-map (kbd "C-c r") 'projectile-rails-command-map)

;; Don't insert file enconding comment at the top
(setq ruby-insert-encoding-magic-comment nil)

;; Don't indent a function args aligned with the opening bracket
(setq ruby-deep-indent-paren nil)
