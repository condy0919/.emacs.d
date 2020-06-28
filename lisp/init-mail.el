;;; init-mail.el --- Read mails in Emacs -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; bundled with system package `mu'
(use-package mu4e
  :ensure nil
  :defer 1
  :commands (mu4e mu4e-compose-new)
  :hook ((mu4e-compose-mode . auto-fill-mode)
         (mu4e-compose-mode . flyspell-mode))
  ;; from doom
  ;; This hook correctly modifies gmail flags on emails when they are marked.
  ;; Without it, refiling (archiving), trashing, and flagging (starring) email
  ;; won't properly result in the corresponding gmail action, since the marks
  ;; are ineffectual otherwise.
  :hook (mu4e-mark-execute-pre . (lambda (mark msg)
                                   (pcase mark
                                     (`trash  (mu4e-action-retag-message msg "-\\Inbox,+\\Trash,-\\Draft"))
                                     (`refile (mu4e-action-retag-message msg "-\\Inbox"))
                                     (`flag   (mu4e-action-retag-message msg "+\\Starred"))
                                     (`unflag (mu4e-action-retag-message msg "-\\Starred")))))
  :custom
  ;; path
  (mu4e-attachment-dir (expand-file-name "~/Mail/.attachments"))

  ;; works better with mbsync
  (mu4e-change-filenames-when-moving t)

  ;; Gmail specific configs
  (mu4e-sent-messages-behavior 'delete)
  (mu4e-index-cleanup nil)
  (mu4e-index-lazy-check t)

  ;; folders & shortcuts
  (mu4e-drafts-folder "/Gmail/Drafts")
  (mu4e-sent-folder   "/Gmail/Sent")
  (mu4e-trash-folder  "/Gmail/Trash")
  (mu4e-refile-folder "/Gmail/All")
  (mu4e-maildir-shortcuts '((:maildir "/Gmail/All"    :key ?a)
                            (:maildir "/Gmail/INBOX"  :key ?i)
                            (:maildir "/Gmail/Sent"   :key ?s)
                            (:maildir "/Gmail/Drafts" :key ?d)
                            (:maildir "/Gmail/Junk"   :key ?j)
                            (:maildir "/Gmail/Trash"  :key ?t)))

  ;; Use fancy icons
  (mu4e-use-fancy-chars t)
  (mu4e-headers-draft-mark     '("D" . ""))
  (mu4e-headers-flagged-mark   '("F" . ""))
  (mu4e-headers-new-mark       '("N" . ""))
  (mu4e-headers-passed-mark    '("P" . ""))
  (mu4e-headers-replied-mark   '("R" . ""))
  (mu4e-headers-seen-mark      '("S" . ""))
  (mu4e-headers-trashed-mark   '("T" . ""))
  (mu4e-headers-attach-mark    '("a" . ""))
  (mu4e-headers-encrypted-mark '("x" . ""))
  (mu4e-headers-signed-mark    '("s" . ""))
  (mu4e-headers-unread-mark    '("u" . ""))

  (mu4e-view-show-addresses t)
  (mu4e-hide-index-messages t)
  (mu4e-view-prefer-html t)

  ;; try to show images
  (mu4e-view-show-images t)
  (mu4e-view-image-max-width 800)
  (mu4e-view-image-max-height 600)

  ;; start with the first (default) context
  (mu4e-context-policy 'pick-first)
  ;; compose with the current context, or ask
  (mu4e-compose-context-policy 'ask-if-none)
  (mu4e-completing-read-function 'ivy-completing-read)

  (mu4e-confirm-quit nil)
  (mu4e-compose-signature nil)
  :config
  ;; Html mails might be better rendered in a browser
  (add-to-list 'mu4e-view-actions '("View in browser" . mu4e-action-view-in-browser))

  ;; Use imagemagick if available
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  ;; General mail settings
  (setq mail-user-agent 'mu4e-user-agent
        user-mail-address "condy0919@gmail.com"
        user-full-name "Zhiwei Chen")
  )

;; Write email with org-mode
(use-package org-mu4e
  :ensure nil
  :hook (mu4e-compose-mode . org-mu4e-compose-org-mode)
  :custom
  (org-mu4e-convert-to-html t))

;; sending mail
(use-package message
  :ensure nil
  :after mu4e
  :custom
  (message-kill-buffer-on-exit t)
  (message-send-mail-function #'smtpmail-send-it))

(use-package smtpmail
  :ensure nil
  :after mu4e
  :custom
  (smtpmail-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-user user-mail-address)
  (smtpmail-smtp-service 587)
  (smptmail-stream-type 'starttls))

(provide 'init-mail)
;;; init-mail.el ends here
