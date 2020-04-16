;;; init-mail.el --- Read mails in Emacs -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; bundled with system package `mu'
(use-package mu4e
  :ensure nil
  :defer 1
  :commands (mu4e mu4e-compose-new)
  :hook (mu4e-compose-mode . auto-fill-mode)
  :custom
  ;; path
  (mu4e-maildir (expand-file-name "~/Mail"))
  (mu4e-attachment-dir (expand-file-name "~/Mail/.attachments"))

  ;; obsolete
  ;; It must be nil
  (mu4e-user-mail-address-list nil)

  (mu4e-user-mailing-lists '(("emacs-china.org"   . "Emacs China")
                             ("aur.archlinux.org" . "Arch Linux")
                             ("github.com"        . "GitHub")
                             ("twitter.com"       . "Twitter")))
  ;; Gmail specific configs
  (mu4e-sent-messages-behavior 'delete)
  (mu4e-index-cleanup nil)
  (mu4e-index-lazy-check t)

  ;; folders & shortcuts
  (mu4e-drafts-folder "/Drafts")
  (mu4e-sent-folder "/Sent")
  (mu4e-trash-folder "/Trash")
  (mu4e-maildir-shortcuts '(("/INBOX" . ?i)
                            ("/Sent" . ?s)
                            ("/Trash" . ?t)
                            ("/Drafts" . ?d)))

  ;; beautiful icons. Copy from doom
  (mu4e-use-fancy-chars t)
  (mu4e-headers-draft-mark '("D" . ""))
  (mu4e-headers-flagged-mark '("F" . ""))
  (mu4e-headers-new-mark '("N" . ""))
  (mu4e-headers-passed-mark '("P" . ""))
  (mu4e-headers-replied-mark '("R" . ""))
  (mu4e-headers-seen-mark '("S" . ""))
  (mu4e-headers-trashed-mark '("T" . ""))
  (mu4e-headers-attach-mark '("a" . ""))
  (mu4e-headers-encrypted-mark '("x" . ""))
  (mu4e-headers-signed-mark '("s" . ""))
  (mu4e-headers-unread-mark '("u" . ""))

  ;; visual-line-mode + auto-fill upon sending
  (mu4e-compose-format-flowed t)
  (mu4e-view-show-addresses t)
  (mu4e-hide-index-messages t)
  ;; try to show images
  (mu4e-view-show-images t)
  ;; use imagemagick if available
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  ;; start with the first (default) context
  (mu4e-context-policy 'pick-first)
  ;; compose with the current context, or ask
  (mu4e-compose-context-policy 'ask-if-none)
  ;; `ivy' integration
  (mu4e-completing-read-function #'ivy-completing-read)
  ;; no need to ask
  (mu4e-confirm-quit nil)
  (mu4e-compose-signature "Sent from my Emacs.")
  :config
  ;; general mail settings
  (setq mail-user-agent 'mu4e-user-agent
        user-mail-address "condy0919@gmail.com"
        user-full-name "Zhiwei Chen")

  ;; evil integration
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key-for-mode 'mu4e-compose-mode
      "s" 'message-send-and-exit
      "d" 'message-kill-buffer
      "S" 'message-dont-send
      "a" 'mail-add-attachment))
  )

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
  (smtpmail-smtp-user "condy0919@gmail.com")
  (smtpmail-smtp-service 587)
  (smptmail-stream-type 'starttls))

(provide 'init-mail)
;;; init-mail.el ends here
