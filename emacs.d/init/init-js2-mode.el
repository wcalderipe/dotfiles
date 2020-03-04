(setq js2-basic-offset 2)
(setq js2-bounce-indent-p nil)

;; Don't need to press tab before typing
(setq  js2-enter-indents-newline t)

;; Fix indenting before moving on.
(setq js2-indent-on-enter-key t)

;; Disable all parse errors and warnings by default,
;; leaving room for flycheck to handle them.
(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)

;; Disable warnings about missing semi-colons.
;; js2-missing-semi-one-line-override t
;; (setq js2-strict-missing-semi-warning nil)

;; Adds highlighting of many Ecma built-in functions.
(setq js2-highlight-level 3)

(setq js2-global-externs
  '("afterAll"
     "afterEach"
     "beforeAll"
     "beforeEach"
     "context"
     "describe"
     "it"))
