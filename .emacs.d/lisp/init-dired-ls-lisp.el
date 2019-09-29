(require 'ls-lisp)

;; Given that certain "ls" switches don't work as expected with the
;; old bin shipped with macOS (tested with latest Mojave), here we
;; start to use the universal ls-lisp mode.
(setq ls-lisp-use-insert-directory-program nil)

;; Do not pass the --dired flag to ls. This CLI option does not work
;; with the ls shipped with macOS.
(setq dired-use-ls-dired nil)

(setq ls-lisp-ignore-case t)
(setq ls-lisp-dirs-first t)

(setq dired-listing-switches "-laAGhF")
