;;; init-reader.el --- The reading stuff -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:

;; RSS reader
(use-package newsticker
  :ensure nil
  :defer t
  :custom
  (newsticker-download-logos nil)
  (newsticker-enable-logo-manipulations nil)
  (newsticker-date-format "%F %a, %T")
  (newsticker-automatically-mark-items-as-old nil)
  (newsticker-automatically-mark-visited-items-as-old t)
  (newsticker-keep-obsolete-items t)
  (newsticker-hide-old-items-in-newsticker-buffer t)
  (newsticker-desc-format nil)
  (newsticker-retrieval-method 'extern)
  (newsticker-frontend 'newsticker-plainview)
  (newsticker-url-list-defaults nil)
  (newsticker-url-list '(("Planet Emacslife" "https://planet.emacslife.com/atom.xml")
                         ("LWN (Linux Weekly News)" "https://lwn.net/headlines/rss"))))

;; The EPub reader
(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode))

;; The builtin doc viewer
(use-package doc-view
  :ensure nil
  :custom
  (doc-view-continuous t))

;; A gopher and a gemini client
(use-package elpher
  :ensure t
  :defer t)

(provide 'init-reader)

;;; init-reader.el ends here
