;;; init-blog.el --- Let's write blogs -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Modified from
;; https://github.com/ksqsf/emacs-config/blob/master/modules/prelude-blog.el

;;; Code:

(defgroup mblog nil
  "Quick manipulation of blog posts."
  :group 'text)

(defcustom mblog-posts-directory (expand-file-name "~/blog/_posts")
  "The directory for your blog posts."
  :group 'mblog
  :type '(string))

(defun mblog--generate-header (title)
  "Customize `TITLE' and generate a header."
  (format "---
layout: post
tilte: %s
categories: note
usemermaid: false
usemathjax: false
---

* content
{:toc}
" title))

(defun mblog-insert-ruby-tag (text ruby)
  "Quick insertion of ruby tag with `TEXT' and `RUBY'."
  (interactive "sText: \nsRuby: \n")
  (insert (format "<ruby>%s<rp>(</rp><rt>%s</rt><rp>)</rp></ruby>"
                  text
                  ruby)))

;;;###autoload
(defun mblog-new-post (title permalink)
  "Create a new blog post with `TITLE' which can be visited by `PERMALINK'."
  (interactive "sTitle: \nsPermalink for post '%s': \n")
  (let* ((time (current-time))
         (date (format-time-string "%Y-%m-%d" time))
         (filename (format "%s-%s.md" date permalink))
         (header (mblog--generate-header title)))
    (find-file (expand-file-name filename mblog-posts-directory))
    (insert header)
    (newline)))

;;;###autoload
(defun mblog-find-post ()
  "Find one of your blog posts."
  (interactive)
  (let ((files (directory-files mblog-posts-directory
                                nil
                                "^[^\\.].*\\.md$")))
    (find-file (expand-file-name (ivy-read "Post: " files)
                                 mblog-posts-directory))))

(with-eval-after-load 'markdown-mode
  (bind-key "C-c C-s r" #'mblog-insert-ruby-tag markdown-mode-map))

(provide 'init-blog)

;;; init-blog.el ends here
