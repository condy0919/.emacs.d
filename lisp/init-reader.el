;;; init-reader.el --- The reading stuff -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:

;; The builtin RSS reader
(use-package newsticker
  :ensure nil
  :hook (newsticker-mode . (lambda ()
                             (setq line-spacing 2)))
  :config
  (with-eval-after-load 'evil-collection
    (evil-set-initial-state 'newsticker-mode 'normal)
    (evil-collection-define-key 'normal 'newsticker-mode-map
      ;; move
      "k" 'newsticker-previous-item
      "j" 'newsticker-next-item
      "gk" 'newsticker-previous-feed
      "gj" 'newsticker-next-feed

      ;; mark
      "r" 'newsticker-mark-item-at-point-as-read
      "i" 'newsticker-mark-item-at-point-as-immortal

      ;; show/hide
      "o" 'newsticker-show-old-items
      "O" 'newsticker-hide-old-items

      ;; refresh
      "gr" 'newsticker-buffer-force-update
      "gR" 'newsticker-get-all-news

      ;; quit
      "q" 'newsticker-close-buffer))
  :custom
  (newsticker-retrieval-method 'extern)
  (newsticker-frontend 'newsticker-plainview)
  (newsticker-download-logos nil)
  (newsticker-enable-logo-manipulations nil)
  (newsticker-date-format "%F %a, %H:%M")
  (newsticker-desc-format "")
  (newsticker-heading-format "%t %d %s")
  (newsticker-html-renderer nil)
  (newsticker-justification nil)
  (newsticker-show-descriptions-of-new-items nil)
  (newsticker-hide-old-items-in-newsticker-buffer t)
  (newsticker-automatically-mark-items-as-old nil)
  (newsticker-automatically-mark-visited-items-as-old t)
  (newsticker-keep-obsolete-items nil)
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
