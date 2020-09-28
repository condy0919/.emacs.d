;;; init-gnus.el --- Focus on reading mailing lists -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package gnus-start
  :ensure nil
  :defer t
  :after gnus
  :custom
  (gnus-startup-file (no-littering-expand-var-file-name "gnus/.newsrc")))

(use-package gnus-sum
  :ensure nil
  :defer t
  :after gnus
  :custom
  (gnus-fetch-old-headers t)
  (gnus-view-pseudo-asynchronously t)
  (gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M")))
  (gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date))
  (gnus-thread-hide-subtree t)
  (gnus-summary-to-prefix        "→")
  (gnus-summary-newsgroup-prefix "⇶")
  ;; Summary thread guides.
  (gnus-sum-thread-tree-indent          "  ")
  (gnus-sum-thread-tree-single-indent   "◎ ")
  (gnus-sum-thread-tree-root            "● ")
  (gnus-sum-thread-tree-false-root      "◌ ")
  (gnus-sum-thread-tree-vertical        "│ ")
  (gnus-sum-thread-tree-leaf-with-other "├─▶ ")
  (gnus-sum-thread-tree-single-leaf     "└─▶ ")
  ;; Mark characters.
  (gnus-score-over-mark  ?↑)
  (gnus-score-below-mark ?↓)
  (gnus-ticked-mark      ?⚑)
  (gnus-dormant-mark     ?⚐)
  (gnus-expirable-mark   ?♻)
  (gnus-read-mark        ?✓)
  (gnus-del-mark         ?✗)
  (gnus-killed-mark      ?☠)
  (gnus-replied-mark     ?⟲)
  (gnus-forwarded-mark   ?⤳)
  (gnus-cached-mark      ?☍)
  (gnus-recent-mark      ?★)
  (gnus-unseen-mark      ?✩)
  (gnus-unread-mark      ?✉)
  (gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references)
  (gnus-summary-display-arrow nil)
  (gnus-build-sparse-threads 'some)
  (gnus-auto-select-first nil))

(use-package gnus-art
  :ensure nil
  :defer t
  :after gnus
  :custom
  (gnus-article-browse-delete-temp-files t)
  (gnus-treat-strip-trailing-blank-lines 'last)
  (gnus-mime-display-multipart-related-as-mixed t))

(use-package gnus-async
  :ensure nil
  :defer t
  :after gnus
  :custom
  (gnus-asynchronous t)
  (gnus-use-header-prefetch t))

(use-package gnus
  :ensure nil
  :defer t
  :custom
  (gnus-select-method '(nnnil))
  (gnus-secondary-select-methods '((nntp "gmane" (nntp-address "news.gmane.io"))
                                   (nntp "news.gwene.org")
                                   (nntp "nntp.lore.kernel.org")))
  (gnus-inhibit-startup-message t)
  (gnus-interactive-exit nil)
  (gnus-use-adaptive-scoring '(word line))
  (gnus-directory (no-littering-expand-var-file-name "gnus/News/"))
  (gnus-article-save-directory (no-littering-expand-var-file-name "gnus/News/")))

(provide 'init-gnus)

;;; init-gnus.el ends here
