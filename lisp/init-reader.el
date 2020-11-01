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
  (define-advice newsticker--cache-read (:around (func &rest args))
    "Read cache data without prompt."
    (cl-letf* (((symbol-function 'y-or-n-p) (lambda (_) t)))
      (apply func args)))
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
                         ("Demonastery" "https://demonastery.org/atom.xml")
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
