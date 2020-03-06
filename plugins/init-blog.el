;;; init-blog.el --- Let's write blogs -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'markdown-mode)

(defun my/markdown-insert-ruby-tag ()
  "Insert ruby tag."
  (interactive)
  (let ((text (read-string "text: "))
        (extra (read-string "extra text: ")))
    (insert (format "<ruby>%s<rp>(</rp><rt>%s</rt><rp>)</rp></ruby>" text extra))))

(bind-key "C-c C-s r" #'my/markdown-insert-ruby-tag markdown-mode-map)

(provide 'init-blog)

;;; init-blog.el ends here
