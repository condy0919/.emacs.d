;;; init-mail.el --- Read mails in Emacs (powered by Gnus) -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; A newsreader in Emacs
(use-package gnus
  :ensure nil
  :custom
  (gnus-use-cache t)
  (gnus-use-scoring nil)
  (gnus-keep-backlog nil)
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

;; Group mode commands for Gnus
(use-package gnus-group
  :ensure nil
  :after gnus
  :hook (gnus-group-mode . gnus-topic-mode)
  :custom
  ;;                                 Ticked    New     Unread  open-status Group
  (gnus-group-line-format "%M%S%p%P %1(%7i%) %3(%7U%) %3(%7y%) %4(%B%-45G%)\n")
  (gnus-group-sort-function '(gnus-group-sort-by-level gnus-group-sort-by-alphabet))
  (gnus-no-groups-message "")
  (gnus-group-goto-unread nil))

;; A folding minor mode for Gnus group buffers
(use-package gnus-topic
  :ensure nil
  :after gnus
  :custom
  (gnus-topic-display-empty-topics nil))

;; Summary mode commands for Gnus
(use-package gnus-sum
  :ensure nil
  :after gnus
  :custom
  (gnus-auto-select-first nil)
  (gnus-auto-select-next nil)
  (gnus-single-article-buffer t)
  (gnus-build-sparse-threads 'some)
  (gnus-view-pseudo-asynchronously t)
  (gnus-simplify-subject-functions '(gnus-simplify-subject-re))
  (gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date))
  (gnus-subthread-sort-functions '(gnus-thread-sort-by-date))
  (gnus-thread-indent-level 2)
  (gnus-thread-hide-subtree nil)
  (gnus-thread-ignore-subject nil)
  (gnus-summary-gather-subject-limit 'fuzzy)
  (gnus-summary-next-group-on-exit nil)
  (gnus-summary-display-while-building t)
  (gnus-summary-ignore-duplicates t)
  (gnus-summary-display-arrow nil))

;; Article mode for Gnus
(use-package gnus-art
  :ensure nil
  :after gnus
  :custom
  ;; No way to slow down my Gnus
  (gnus-treat-from-picon nil)
  (gnus-treat-mail-picon nil)
  (gnus-treat-newsgroups-picon nil)
  (gnus-treat-newsgroups-picon nil)
  (gnus-treat-from-gravatar nil)
  (gnus-treat-mail-gravatar nil)
  (gnus-treat-body-boundary nil)
  ;; Block images
  (gnus-blocked-images ".")
  (gnus-article-sort-functions '((not gnus-article-sort-by-number)
                                 (not gnus-article-sort-by-date)))
  (gnus-article-show-cursor t)
  (gnus-article-browse-delete-temp t)
  (gnus-mime-display-multipart-as-mixed t)
  (gnus-mime-display-multipart-related-as-mixed t)
  (gnus-mime-display-multipart-alternative-as-mixed t))

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
  (gnus-check-new-newsgroups nil)
  (gnus-use-dribble-file t)
  (gnus-always-read-dribble-file t)
  (gnus-save-newsrc-file nil)
  (gnus-read-newsrc-file nil)
  (gnus-subscribe-newsgroup-method 'gnus-subscribe-interactively))

;; Unplugged support for Gnus
(use-package gnus-agent
  :ensure nil
  :after gnus
  :custom
  (gnus-agent-mark-unread-after-downloaded t)
  (gnus-agent-cache t)
  (gnus-agent-go-online t)
  (gnus-agent-expire-all nil)
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
  (gnus-cache-enter-articles '(ticked dormant unread))
  (gnus-cache-remove-articles '(read))
  (gnus-cacheable-groups "^\\(nntp\\|nnimap\\)"))

;; Send notifications on new messages in Gnus
(use-package gnus-notifications
  :ensure nil
  :after gnus
  :custom
  (gnus-notifications-minimum-level 1)
  (gnus-notifications-use-gravatar nil)
  (gnus-notifications-use-google-contacts nil))

;; Bookmarks in Gnus
(use-package gnus-bookmark
  :ensure nil
  :after gnus
  :custom
  (gnus-bookmark-bookmark-inline-details '(subject author)))

;; Mail support functions for the Gnus mail backends
(use-package nnmail
  :ensure nil
  :after gnus
  :custom
  (nnimap-expiry-wait 'never)
  (nnmail-expiry-target "nnml:expired")
  (nnmail-expiry-wait 'never)
  (nnmail-split-methods 'nnmail-split-fancy)
  (nnmail-treat-duplicates 'delete))

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
  (message-signature user-full-name))

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
