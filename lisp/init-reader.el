;;; init-reader.el --- The reading staff -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:

;; Notes manager
(use-package deft
  :ensure t
  :defines (org-directory)
  :bind ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  ;; Disable auto save
  (deft-auto-save-interval 0)
  (deft-extensions '("org" "md"))
  (deft-directory org-directory)
  (deft-use-filename-as-title t)
  (deft-use-filter-string-for-filename t)
  (deft-file-naming-rules '((noslash . "-")
                            (nospace . "-")
                            (case-fn . downcase))))


;; RSS reader
;; The builtin newsticker is buggy
(use-package elfeed
  :ensure t
  :bind ("C-x 4 n" . elfeed)
  :custom
  (elfeed-feeds '(("https://planet.emacslife.com/atom.xml" emacs)
                  ("https://lwn.net/headlines/rss" linux)))
  (elfeed-search-title-max-width 100))

;; The EPub reader
(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode))

(provide 'init-reader)

;;; init-reader.el ends here
