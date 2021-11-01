;;; pkg-org-reading-list.el --- -*- lexical-binding: t; -*-

;;; Code:
(require 'org)
(require 'org-ql)
(require 'org-element)

;; (defvar org-reading-list-buffer-name
;;   "*reading-list*")

;; (defun org-reading-list--get-buffer-create ()
;;   (with-current-buffer (get-buffer-create org-reading-list-buffer-name)
;;     (org-mode)))

(defun org-reading-list ()
  (interactive)
  (org-ql-search
    (org-agenda-files)
    '(property "STATUS")
    :title "Reading List"
    :buffer "*reading-list*"
    :sort '(lambda (a b)
             (string> (org-element-property :CREATED_AT a)
                      (org-element-property :CREATED_AT b)))
    :super-groups '((:name "Unread" :property ("STATUS" "unread"))
                    (:name "Read" :property ("STATUS" "read")))))

(provide 'pkg-org-reading-list)

;;; pkg-org-reading-list.el ends here
