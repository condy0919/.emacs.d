;;; init-bib.el --- Bibliograph management  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(use-package ebib
  :ensure t
  :commands ebib
  :custom
  (ebib-use-timestamp t)
  (ebib-bib-search-dirs '("~/.org"))
  (ebib-preload-bib-files '("papers.bib"))
  (ebib-notes-directory '("~/.org")))

(provide 'init-bib)
;;; init-bib.el ends here
