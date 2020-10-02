;;; init-mail.el --- Read mails in Emacs (powered by Gnus) -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; A newsreader in Emacs
;; TODO REVISE
(use-package gnus
  :ensure nil
  :custom
  (gnus-use-cache t)
  (gnus-use-scoring nil)
  (gnus-suppress-duplicates t)
  (gnus-novice-user nil)
  (gnus-interactive-exit nil)
  (gnus-interactive-catchup nil)
  (gnus-use-cross-reference nil)
  (gnus-inhibit-startup-message nil)
  (gnus-home-directory (no-littering-expand-var-file-name "gnus/"))
  (gnus-select-method '(nnimap "GMail"
                               (nnimap-address "imap.gmail.com")
                               (nnimap-inbox "INBOX")
                               (nnimap-expunge t)
                               (nnimap-server-port 993)
                               (nnimap-stream ssl)))
  (gnus-secondary-select-methods '((nntp "gmane" (nntp-address "news.gmane.io"))
                                   (nntp "news.gwene.org")
                                   (nntp "nntp.lore.kernel.org"))))

;; Article mode for Gnus
;; TODO REVISE
(use-package gnus-art
  :ensure nil
  :after gnus
  :custom
  (gnus-article-browse-delete-temp t)
  (gnus-treat-strip-trailing-blank-lines 'last)
  (gnus-mime-display-multipart-related-as-mixed t))

;; Asynchronous support for Gnus
(use-package gnus-async
  :ensure nil
  :after gnus
  :custom
  (gnus-asynchronous t)
  (gnus-use-header-prefetch t))

;; Startup functions for Gnus
(use-package gnus-start
  :ensure nil
  :after gnus
  :custom
  (gnus-use-dribble-file t)
  (gnus-always-read-dribble-file t)
  (gnus-save-newsrc-file nil)
  (gnus-read-newsrc-file nil)
  (gnus-check-new-newsgroups nil)
  (gnus-subscribe-newsgroup-method 'gnus-subscribe-interactively))

;; Unplugged support for Gnus
(use-package gnus-agent
  :ensure nil
  :after gnus
  :custom
  (gnus-agent-expire-days 30))

;; Mail and post interface for Gnus
(use-package gnus-msg
  :ensure nil
  :after gnus
  :custom
  (gnus-gcc-mark-as-read t))

;; Cache interface for Gnus
(use-package gnus-cache
  :ensure nil
  :after gnus
  :custom
  (gnus-cache-enter-articles '(ticked dormant read unread))
  (gnus-cache-remove-articles nil)
  (gnus-cacheable-groups "^\\(nntp\\|nnimap\\)"))

(use-package gnus-sum
  :ensure nil
  :after gnus
  :custom
  (gnus-fetch-old-headers t)
  (gnus-view-pseudo-asynchronously t)
  (gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M")))
  (gnus-thread-sort-functions '(gnus-thread-sort-by-date))
  (gnus-thread-hide-subtree t)
  (gnus-thread-indent-level 1)
  (gnus-summary-ignore-duplicates t)
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
  (gnus-simplify-subject-functions '(gnus-simplify-subject-re gnus-simplify-whitespace))
  (gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references)
  (gnus-summary-display-arrow nil)
  (gnus-build-sparse-threads 'some)
  (gnus-auto-select-first nil))

;; Composing mail and news messages
(use-package message
  :ensure nil
  :custom
  (user-full-name "Zhiwei Chen")
  (user-mail-address "condy0919@gmail.com")
  (message-kill-buffer-on-exit t)
  (message-use-mail-followup-to nil)
  (message-mail-alias-type 'ecomplete)
  (message-wide-reply-confirm-recipients t)
  (message-send-mail-function #'smtpmail-send-it)
  (message-signature (concat "\n\n-- \n" user-full-name)))

;; Sending mails
(use-package smtpmail
  :ensure nil
  :custom
  (smtpmail-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-user user-mail-address)
  (smtpmail-smtp-service 587)
  (smptmail-stream-type 'starttls))


(provide 'init-mail)
;;; init-mail.el ends here
