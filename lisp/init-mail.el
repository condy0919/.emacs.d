;;; init-mail.el --- Read mails in Emacs (powered by Gnus) -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(eval-when-compile
  (require 'rx))

;; A newsreader in Emacs
(use-package gnus
  :ensure nil
  :custom
  (gnus-use-cache t)
  (gnus-use-scoring nil)
  (gnus-keep-backlog 10)
  (gnus-suppress-duplicates t)
  (gnus-novice-user nil)
  (gnus-expert-user t)
  (gnus-interactive-exit 'quiet)
  (gnus-dbus-close-on-sleep t)
  (gnus-use-cross-reference nil)
  (gnus-inhibit-startup-message nil)
  (gnus-home-directory (no-littering-expand-var-file-name "gnus/"))
  (gnus-select-method '(nnimap "GMail"
                               (nnimap-address "imap.gmail.com")
                               (nnimap-server-port "imaps")
                               (nnimap-stream ssl)
                               (nnimap-expunge 'on-exit)
                               (nnimap-streaming t)
                               (nnimap-fetch-partial-articles "text/")
                               (nnimap-record-commands t)
                               (nnmail-expiry-target "nnimap+GMail:[Gmail]/Trash")
                               (nnir-search-engine imap)
                               ;; Client-Side settings
                               (nnimap-inbox "INBOX")))
  (gnus-secondary-select-methods '((nntp "gmane" (nntp-address "news.gmane.io"))
                                   (nntp "nntp.lore.kernel.org"))))

;; Group mode commands for Gnus
(use-package gnus-group
  :ensure nil
  :defines gnus-tmp-group
  :after gnus
  :hook ((gnus-group-mode . gnus-topic-mode)
         (gnus-select-group . gnus-group-set-timestamp))
  :config
  (defun gnus-user-format-function-d (_)
    (let ((time (gnus-group-timestamp gnus-tmp-group)))
      (if time
          (format-time-string "%F %H:%M" time)
        "")))
  :custom-face
  (gnus-group-mail-1         ((t (:foreground "DeepPink1" :bold t))))
  (gnus-group-mail-1-empty   ((t (:foreground "DeepPink4" :italic t))))
  (gnus-group-mail-2         ((t (:foreground "HotPink1" :bold t))))
  (gnus-group-mail-2-empty   ((t (:foreground "HotPink4" :italic t))))
  (gnus-group-mail-3         ((t (:foreground "magenta1" :bold t))))
  (gnus-group-mail-3-empty   ((t (:foreground "magenta4" :italic t))))
  (gnus-group-mail-low       ((t (:foreground "SteelBlue1" :bold t))))
  (gnus-group-mail-low-empty ((t (:foreground "SteelBlue4" :italic t))))
  (gnus-group-news-1         ((t (:foreground "DarkSeaGreen1" :bold t))))
  (gnus-group-news-1-empty   ((t (:foreground "DarkSeaGreen4" :italic t))))
  (gnus-group-news-2         ((t (:foreground "CadetBlue1" :bold t))))
  (gnus-group-news-2-empty   ((t (:foreground "CadetBlue4" :italic t))))
  (gnus-group-news-3         ((t (:foreground "RoyalBlue1" :bold t))))
  (gnus-group-news-3-empty   ((t (:foreground "RoyalBlue4" :italic t))))
  (gnus-group-news-low       ((t (:foreground "SkyBlue1" :bold t))))
  (gnus-group-news-low-empty ((t (:foreground "SkyBlue4" :italic t))))
  :custom
  ;;          indentation ------------.
  ;;  #      process mark ----------. |
  ;;                level --------. | |
  ;;           subscribed ------. | | |
  ;;  %          new mail ----. | | | |
  ;;  *   marked articles --. | | | | |
  ;;                        | | | | | |  Ticked    New     Unread  open-status Group
  (gnus-group-line-format "%M%m%S%L%p%P %1(%7i%) %3(%7U%) %3(%7y%) %4(%B%-45G%) %ud\n")
  (gnus-group-sort-function '(gnus-group-sort-by-level gnus-group-sort-by-alphabet))
  (gnus-group-highlight '(;; Mail
                          ((and mailp (eq level 1) (= unread 0)) . gnus-group-mail-1-empty)
                          ((and mailp (eq level 1))              . gnus-group-mail-1)
                          ((and mailp (eq level 2) (= unread 0)) . gnus-group-mail-2-empty)
                          ((and mailp (eq level 2))              . gnus-group-mail-2)
                          ((and mailp (eq level 3) (= unread 0)) . gnus-group-mail-3-empty)
                          ((and mailp (eq level 3))              . gnus-group-mail-3)
                          ((and mailp              (= unread 0)) . gnus-group-mail-low-empty)
                          ((and mailp)                           . gnus-group-mail-low)
                          ;; News
                          ((and (eq level 1) (= unread 0)) . gnus-group-news-1-empty)
                          ((and (eq level 1))              . gnus-group-news-1)
                          ((and (eq level 2) (= unread 0)) . gnus-group-news-2-empty)
                          ((and (eq level 2))              . gnus-group-news-2)
                          ((and (eq level 3) (= unread 0)) . gnus-group-news-3-empty)
                          ((and (eq level 3))              . gnus-group-news-3)
                          ((and              (= unread 0)) . gnus-group-news-low-empty)
                          (t                               . gnus-group-news-low))))

;; A folding minor mode for Gnus group buffers
(use-package gnus-topic
  :ensure nil
  :after gnus
  :custom
  (gnus-topic-indent-level 2)
  (gnus-topic-display-empty-topics t))

;; Summary mode commands for Gnus
(use-package gnus-sum
  :ensure nil
  :after gnus
  :custom
  ;; Pretty marks
  (gnus-sum-thread-tree-root            "┌ ")
  (gnus-sum-thread-tree-false-root      "◌ ")
  (gnus-sum-thread-tree-single-indent   "◎ ")
  (gnus-sum-thread-tree-vertical        "│")
  (gnus-sum-thread-tree-indent          "  ")
  (gnus-sum-thread-tree-leaf-with-other "├─►")
  (gnus-sum-thread-tree-single-leaf     "╰─►")
  (gnus-summary-line-format "%U%R %3d %[%-23,23f%] %B %s\n")
  ;; Loose threads
  (gnus-summary-make-false-root 'adopt)
  (gnus-simplify-subject-functions '(gnus-simplify-subject-re gnus-simplify-whitespace))
  (gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject)
  ;; Filling in threads
  ;; 2 old articles are enough for context
  (gnus-fetch-old-headers 2)
  (gnus-fetch-old-ephemeral-headers 2)
  (gnus-build-sparse-threads 'some)
  ;; More threading
  (gnus-show-threads t)
  (gnus-thread-indent-level 2)
  (gnus-thread-hide-subtree nil)
  (gnus-sort-gathered-threads-function 'gnus-thread-sort-by-date)
  ;; Sorting
  (gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date))
  (gnus-subthread-sort-functions '(gnus-thread-sort-by-date))
  ;; Viewing
  (gnus-view-pseudos 'automatic)
  (gnus-view-pseudos-separately t)
  (gnus-view-pseudo-asynchronously t)
  ;; No auto select
  (gnus-auto-select-first nil)
  (gnus-auto-select-next nil)
  (gnus-paging-select-next nil)
  ;; Misc
  (gnus-summary-ignore-duplicates t)
  (gnus-summary-display-while-building t))

;; Article mode for Gnus
(use-package gnus-art
  :ensure nil
  :after gnus
  :custom
  ;; No way to slow down my Gnus
  (gnus-treat-from-picon nil)
  (gnus-treat-mail-picon nil)
  (gnus-treat-newsgroups-picon nil)
  (gnus-treat-from-gravatar nil)
  (gnus-treat-mail-gravatar nil)
  (gnus-treat-body-boundary nil)
  (gnus-treat-display-x-face nil)
  (gnus-treat-display-face nil)
  (gnus-visible-headers (rx line-start (or "From"
                                           "Subject"
                                           "Mail-Followup-To"
                                           "Date"
                                           "To"
                                           "Cc"
                                           "Newsgroups"
                                           "User-Agent"
                                           "X-Mailer"
                                           "X-Newsreader")
                            ":"))
  ;; Block images
  (gnus-inhibit-images t)
  (gnus-article-sort-functions '((not gnus-article-sort-by-number)
                                 (not gnus-article-sort-by-date)))
  (gnus-article-show-cursor t)
  (gnus-article-browse-delete-temp t))

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
  ;; No, thanks
  (gnus-check-new-newsgroups nil)
  (gnus-save-killed-list nil)
  ;; Record Gnus data (reading articles, killing/subscribing groups)
  (gnus-use-dribble-file t)
  (gnus-always-read-dribble-file t)
  (gnus-save-newsrc-file nil)
  (gnus-read-newsrc-file nil)
  (gnus-subscribe-newsgroup-method 'gnus-subscribe-interactively))

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

;; Search in Gnus
(use-package gnus-search
  :ensure nil
  :when (>= emacs-major-version 28)
  :after gnus
  :custom
  (gnus-search-use-parsed-queries t))

;; Extract (uu)encoded files in Gnus
(use-package gnus-uu
  :ensure nil
  :after gnus
  :custom
  (gnus-uu-kill-carriage-return t)
  (gnus-uu-ignore-files-by-type "audio/\\|video/mpeg"))

;; Mail support functions for the Gnus mail backends
(use-package nnmail
  :ensure nil
  :after gnus
  :custom
  (nnmail-expiry-wait 30)
  (nnmail-split-methods 'nnmail-split-fancy)
  (nnmail-treat-duplicates 'delete))

;; Composing mail and news messages
;;
;; When use `ecomplete', <kbd>M-n</kbd> and <kbd>M-p</kbd> can be used for mail
;; address selection.
(use-package message
  :ensure nil
  :hook (message-mode . auto-fill-mode)
  :custom
  (user-full-name "Zhiwei Chen")
  (user-mail-address "condy0919@gmail.com")
  (message-kill-buffer-on-exit t)
  (message-mail-alias-type 'ecomplete)
  (message-send-mail-function #'message-use-send-mail-function)
  (message-signature user-full-name))

(use-package sendmail
  :ensure nil
  :custom
  (send-mail-function #'smtpmail-send-it))

;; Sending mails
(use-package smtpmail
  :ensure nil
  :custom
  (smtpmail-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-user user-mail-address)
  (smtpmail-smtp-service 587)
  (smptmail-stream-type 'ssl))

(provide 'init-mail)
;;; init-mail.el ends here
