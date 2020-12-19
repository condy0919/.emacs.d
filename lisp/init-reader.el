;;; init-reader.el --- The reading stuff -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:

;; The builtin RSS reader
(use-package newsticker
  :ensure nil
  :custom-face
  (newsticker-treeview-old-face ((nil (:inherit 'newsticker-treeview-face :foreground "#7c7c75"))))
  :custom
  ;; Keep obsolete items for a month
  (newsticker-keep-obsolete-items t)
  (newsticker-obsolete-item-max-age (* 30 86400))
  ;; Sane behavior
  (newsticker-automatically-mark-items-as-old nil)
  (newsticker-automatically-mark-visited-items-as-old t)
  ;; No logos
  (newsticker-download-logos nil)
  (newsticker-enable-logo-manipulations nil)
  ;; Emacs async sucks
  (newsticker-retrieval-method 'extern)
  ;; Improve the default display
  (newsticker-treeview-listwindow-height 20)
  (newsticker-treeview-date-format "%F %a, %H:%M  ")
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
  (doc-view-continuous t)
  (doc-view-scale-internally nil))

(provide 'init-reader)
;;; init-reader.el ends here
