(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))

(require 'rspec-mode)

;; Turn on projectile-ruby when ruby-mode is on
(add-hook 'ruby-mode-hook 'projectile-rails-global-mode)

;; Robe features only work when there's a Ruby subprocess running in
;; the background. In order to start the process the fn `robe-start`
;; have to be called.
;; On the fly auto complete for Ruby related buffers.
(eval-after-load 'company
  '(push 'company-robe company-backends))

;; TODO: Add `[` and `]` in order to fold big arrays. For some reason
;; an array like the one described below raise an error in the marker.
;;
;; arr = [
;;   1,
;;   2,
;;   3
;; ]
;;
;; At the time I'm writing this comment I'm not sure if folding arrays
;; will be used enough to justify the time I'm spending on this. I'll
;; reevaluate the need of it in the future.
(add-to-list 'hs-special-modes-alist
             '(ruby-mode "\\(def\\|class\\|module\\|do\\|if\\|else\\|unless\\|{\\)"
                         "\\(end\\|}\\)"
                         "#"
                         nil
                         nil))


;; Don't insert file enconding comment at the top
(setq ruby-insert-encoding-magic-comment nil)

;; Don't indent a function args aligned with the opening bracket
(setq ruby-deep-indent-paren nil)
