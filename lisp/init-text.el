;;; init-text.el --- Writing -*- lexical-binding: t -*-

;;; Commentary:
;;
;; `org-mode' is too huge to place here.

;;; Code:

;; Pixel alignment for org/markdown tables
(use-package valign
  :ensure t
  :hook ((markdown-mode org-mode) . valign-mode))

;; Jekyll blog posts manager
(use-package jblog
  :ensure t
  :quelpa (jblog :fetcher github :repo "condy0919/jblog")
  :commands jblog
  :config
  (with-eval-after-load 'evil-collection
    (evil-set-initial-state 'jblog-mode 'normal)
    (evil-collection-define-key 'normal 'jblog-mode-map
      "C" 'jblog-create
      "D" 'jblog-delete
      "O" 'jblog-open-posts-directory
      "s" 'jblog-search
      "gr" 'jblog-refresh))
  :custom
  (jblog-posts-directory (expand-file-name "~/blog/_posts"))
  (jblog-post-headers [("Date"       12 t)
                       ("Title"      36 t)
                       ("Categories" 25 t)
                       ("Tags"       25 t)])
  (jblog-post-headers-format "---
layout: post
title: %s
categories: note
tags: note
usemermaid: false
usemathjax: false
---

* content
{:toc}
"))

;; The markdown mode is awesome! unbeatable
(use-package markdown-mode
  :ensure t
  :init
  (advice-add #'markdown--command-map-prompt :override #'ignore)
  (advice-add #'markdown--style-map-prompt   :override #'ignore)
  :mode ("README\\(?:\\.md\\)?\\'" . gfm-mode)
  :hook (markdown-mode . visual-line-mode)
  :bind (:map markdown-mode-style-map
         ("r" . markdown-insert-ruby-tag)
         ("d" . markdown-insert-details))
  :config
  (defun markdown-insert-ruby-tag (text ruby)
    "Insert ruby tag with `TEXT' and `RUBY' quickly."
    (interactive "sText: \nsRuby: \n")
    (insert (format "<ruby>%s<rp>(</rp><rt>%s</rt><rp>)</rp></ruby>" text ruby)))

  (defun markdown-insert-details (title)
    "Insert details tag (collapsible) quickly."
    (interactive "sTitle: ")
    (insert (format "<details><summary>%s</summary>\n\n</details>" title)))

  (with-eval-after-load 'evil-collection
    (evil-collection-define-key 'normal 'markdown-mode-map
      (kbd "<tab>") 'markdown-cycle
      (kbd "S-<tab>") 'markdown-shifttab))
  :custom
  (markdown-header-scaling t)
  (markdown-enable-wiki-links t)
  (markdown-italic-underscore t)
  (markdown-asymmetric-header t)
  (markdown-gfm-uppercase-checkbox t)
  (markdown-fontify-code-blocks-natively t))

;; ReStructuredText
(use-package rst
  :ensure nil
  :hook ((rst-mode . visual-line-mode)
         (rst-adjust . rst-toc-update)))

(provide 'init-text)
;;; init-text.el ends here
