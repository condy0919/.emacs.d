;;; init-news.el --- gnus -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:

(use-package gnus
  :ensure nil
  :defer t
  :custom
  (gnus-visible-headers "^From:\\|^Reply-To\\|^Organization:\\|^To:\\|^Cc:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Gnus")
  (gnus-sorted-header-list
   '("^From:" "^Reply-To" "^Organization:" "^To:" "^Cc:" "^Newsgroups:"
     "^Subject:" "^Date:" "^Gnus"))
  (gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B (%c) %s%)\n")
  (gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M")))
  (gnus-group-line-format "%M%S%p%P%5y:%B %G\n") ;;"%B%(%g%)"
  (gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references)
  (gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date))
  (gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\”]\”[#’()]")
  (gnus-sum-thread-tree-false-root "")
  (gnus-sum-thread-tree-indent " ")
  (gnus-sum-thread-tree-leaf-with-other "├► ")
  (gnus-sum-thread-tree-root "")
  (gnus-sum-thread-tree-single-leaf "╰► ")
  (gnus-sum-thread-tree-vertical "│")
  (gnus-article-browse-delete-temp t)
  (gnus-treat-strip-trailing-blank-lines 'last)
  (gnus-keep-backlog 'nil)
  (gnus-summary-display-arrow nil) ; Don't show that annoying arrow:
  (gnus-mime-display-multipart-related-as-mixed t) ; Show more MIME-stuff:
  (gnus-auto-select-first nil) ; Don't get the first article automatically:
  (smiley-style 'medium)
  (gnus-keep-backlog '0))

(provide 'init-news)
;;; init-news.el ends here
