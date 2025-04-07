;;; init-text.el --- Writing -*- lexical-binding: t -*-

;;; Commentary:
;;
;; `org-mode' is too huge to place here.

;;; Code:

;; Pixel alignment for org/markdown tables
(use-package valign
  :ensure t
  :hook ((markdown-mode org-mode) . valign-mode))

;; The markdown mode is awesome! unbeatable
(use-package markdown-mode
  :ensure t
  :mode ("README\\(?:\\.md\\)?\\'" . gfm-mode)
  :hook (markdown-mode . visual-line-mode)
  :bind (:map markdown-mode-style-map
         ("r" . markdown-insert-ruby-tag))
  :config
  (defun markdown-insert-ruby-tag (text ruby)
    "Insert ruby tag with `TEXT' and `RUBY' quickly."
    (interactive "sText: \nsRuby: \n")
    (insert (format "<ruby>%s<rp>(</rp><rt>%s</rt><rp>)</rp></ruby>" text ruby)))
  :custom
  (markdown-header-scaling t)
  (markdown-enable-wiki-links t)
  (markdown-italic-underscore t)
  (markdown-asymmetric-header t)
  (markdown-gfm-uppercase-checkbox t)
  (markdown-enable-prefix-prompts nil)
  (markdown-fontify-code-blocks-natively t))

;; ReStructuredText
(use-package rst
  :ensure nil
  :hook ((rst-mode . visual-line-mode)
         (rst-adjust . rst-toc-update)))

(provide 'init-text)
;;; init-text.el ends here
