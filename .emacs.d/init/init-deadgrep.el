(advice-add 'deadgrep-visit-result :after #'my/scroll-line-to-center)

;; Update jump list before leaving the deadgrep buffer.
(evil-add-command-properties #'deadgrep-visit-result :jump t)
