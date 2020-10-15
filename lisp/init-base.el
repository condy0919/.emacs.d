;;; init-base.el --- The necessary settings -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(eval-when-compile
  (require 'cl-macs)
  (require 'init-macros))

;; Supress GUI features
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-default-init t
      inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-buffer-menu t)

;; Linux specific
(setq x-gtk-use-system-tooltips nil
      x-underline-at-descent-line t)

;; Ignore errors on selecting text
(my/ignore-errors-for x-get-selection-internal)

;; MacOS specific
(when (eq system-type 'darwin)
  ;; Make titlebar dark
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

  (setq delete-by-moving-to-trash t)

  ;; Curse Lion and its sudden but inevitable fullscreen mode!
  ;; NOTE Meaningless to railwaycat's emacs-mac build
  (setq ns-use-native-fullscreen nil)

  ;; Visit files opened outside of Emacs in existing frame, not a new one
  (setq ns-pop-up-frames nil)

  (setq mac-option-modifier 'hyper
        mac-command-modifier 'meta)

  ;; I heavily use frame in MacOS
  (global-set-key (kbd "H-p") 'ns-prev-frame)
  (global-set-key (kbd "H-n") 'ns-next-frame)
  (global-set-key (kbd "H-g") 'select-frame-by-name)

  ;; CUA for MacOS
  (global-set-key (kbd "H-v") 'clipboard-yank)
  (global-set-key (kbd "H-c") 'clipboard-kill-ring-save)
  (global-set-key (kbd "H-x") 'clipboard-kill-region))

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Optimize for very long lines
(setq bidi-paragraph-direction 'left-to-right
      bidi-inhibit-bpa t)

;; No backup files
(setq make-backup-files nil
      auto-save-default nil)

;; No lock files
(setq create-lockfiles nil)

;; Always load the newest file
(setq load-prefer-newer t)

;; Make the prompt of "*Python*" buffer readonly
(setq comint-prompt-read-only t)

;; Cutting and pasting use primary/clipboard
(setq select-enable-primary t
      select-enable-clipboard t)

;; No gc for font caches
(setq inhibit-compacting-font-caches t)

;; No annoying bell
(setq ring-bell-function 'ignore)

;; No eyes distraction
(when (fboundp 'blink-cursor-mode)
  (blink-cursor-mode -1))

;; Smooth scroll & friends
(setq scroll-step 2
      scroll-margin 2
      hscroll-step 2
      hscroll-margin 2
      scroll-conservatively 101
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position 'always)

;; Disable auto vertical scroll for tall lines
(setq auto-window-vscroll nil)

;; Newline behaviour
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "S-<return>") 'comment-indent-new-line)

;; Dont move points out of eyes
(setq mouse-yank-at-point t)

(setq-default fill-column 80)

;; Treats the `_' as a word constituent
(add-hook 'after-change-major-mode-hook
          (lambda ()
            (modify-syntax-entry ?_ "w")))

;; No tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Font size
(set-face-attribute 'default nil :height 110)

;; Prefer shorter names
(fset 'yes-or-no-p 'y-or-n-p)

(defalias 'list-buffers 'ibuffer)

;; A simple frame title
(setq frame-title-format '("%b - Emacs")
      icon-title-format frame-title-format)

;; Enable the disabled narrow commands
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Enable the disabled dired commands
(put 'dired-find-alternate-file 'disabled nil)

;; Enable the disabled `list-timers', `list-threads' commands
(put 'list-timers 'disabled nil)
(put 'list-threads 'disabled nil)

;; Use TeX as default IM
(setq default-input-method "TeX")

;; Keep clean but enable `menu-bar' in MacOS
(when (and (fboundp 'menu-bar-mode) (not (eq system-type 'darwin)))
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

;; Highlight parenthesises
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

;; The selected region of text can be deleted
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; Show line/column number
(use-package simple
  :ensure nil
  :custom
  ;; show line/column/filesize in modeline
  (line-number-mode t)
  (column-number-mode t)
  (size-indication-mode t)
  ;; confusing if no fringes
  (visual-line-fringe-indicators '(nil right-curly-arrow))
  ;; column starts from 1
  (column-number-indicator-zero-based nil)
  ;; save current clipboard text
  (save-interprogram-paste-before-kill t)
  ;; eliminate duplicates
  (kill-do-not-save-duplicates t)
  ;; include '\n'
  (kill-whole-line t)
  ;; show the name of character in `what-cursor-position'
  (what-cursor-show-names t))

;; Type text
(use-package text-mode
  :ensure nil
  :custom
  ;; better word wrapping for CJK characters
  (word-wrap-by-category t)
  ;; paragraphs
  (sentence-end "\\([，。、！？]\\|……\\|[,.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
  (sentence-end-double-space nil))

;; Back to the previous position
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

;; Update buffer whenever file changes
;; Also revert dired buffer.
(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode)
  :custom
  (auto-revert-interval 3)
  (auto-revert-avoid-polling t)
  (auto-revert-verbose nil)
  (auto-revert-remote-files t)
  (auto-revert-check-vc-info t)
  (global-auto-revert-non-file-buffers t))

;; Highlight current line
(use-package hl-line
  :ensure nil
  :hook (after-init . global-hl-line-mode))

;; Switch window
(use-package window
  :ensure nil
  :bind (("M-o" . other-window)
         ("M-O" . other-window-backward))
  :config
  (defun other-window-backward ()
    (interactive)
    (other-window -1)))

;; Server mode.
;; Use emacsclient to connect
(use-package server
  :ensure nil
  :when (display-graphic-p)
  :defer 1
  :config
  (unless (server-running-p)
    (server-start)))

;; Workaround with minified source files
(use-package so-long
  :ensure nil
  :when (>= emacs-major-version 27)
  :hook (after-init . global-so-long-mode))

;; Make escape more nature
(use-package minibuffer
  :ensure nil
  :bind (:map minibuffer-local-map
         ([escape] . abort-recursive-edit)
         :map minibuffer-local-ns-map
         ([escape] . abort-recursive-edit)
         :map minibuffer-local-completion-map
         ([escape] . abort-recursive-edit)
         :map minibuffer-local-must-match-map
         ([escape] . abort-recursive-edit)
         :map minibuffer-local-isearch-map
         ([escape] . abort-recursive-edit))
  :custom
  (minibuffer-eldef-shorten-default t)
  (minibuffer-electric-default-mode t)
  (minibuffer-depth-indicate-mode t)
  (enable-recursive-minibuffers t))

;; Holidays
(use-package calendar
  :ensure nil
  :defines org-agenda-diary-file
  :hook (calendar-today-visible . calendar-mark-today)
  :config
  (define-advice org-agenda-add-entry-to-org-agenda-diary-file (:after (_type text &optional _d1 _d2))
    (when (string-match "\\S-" text)
      (with-current-buffer (find-file-noselect org-agenda-diary-file)
        (save-buffer))))
  :custom
  (calendar-chinese-all-holidays-flag t)
  (holiday-local-holidays `((holiday-fixed 3 8  "Women's Day")
                            (holiday-fixed 3 12 "Arbor Day")
                            ,@(cl-loop for i from 1 to 3
                                       collect `(holiday-fixed 5 ,i "International Workers' Day"))
                            (holiday-fixed 5 4  "Chinese Youth Day")
                            (holiday-fixed 6 1  "Children's Day")
                            (holiday-fixed 9 10 "Teachers' Day")
                            ,@(cl-loop for i from 1 to 7
                                       collect `(holiday-fixed 10 ,i "National Day"))
                            (holiday-fixed 10 24 "Programmers' Day")
                            (holiday-fixed 11 11 "Singles' Day")))
  (holiday-other-holidays '((holiday-fixed 4 22 "Earth Day")
                            (holiday-fixed 4 23 "World Book Day")
                            (holiday-sexp '(if (or (zerop (% year 400))
                                                   (and (% year 100) (zerop (% year 4))))
                                               (list 9 12 year)
                                             (list 9 13 year))
                                          "World Programmers' Day")
                            (holiday-fixed 10 10 "World Mental Health Day")))
  (calendar-holidays `(,@holiday-general-holidays
                       ,@holiday-oriental-holidays
                       ,@holiday-other-holidays
                       ,@holiday-local-holidays))
  (calendar-mark-holidays-flag t)
  (calendar-mark-diary-entries-flag nil)
  (calendar-time-zone-style 'numeric)
  ;; start from Monday
  (calendar-week-start-day 1)
  ;; year/month/day
  (calendar-date-style 'iso))

;; Appointment
(use-package appt
  :ensure nil
  :hook (after-init . appt-activate)
  :config
  (defun appt-display-with-notification (min-to-app new-time appt-msg)
    (notify-send :title (format "Appointment in %s minutes" min-to-app)
                 :body appt-msg
                 :urgency 'critical)
    (appt-disp-window min-to-app new-time appt-msg))
  :custom
  (appt-audible nil)
  (appt-display-diary nil)
  (appt-display-interval 5)
  (appt-display-mode-line t)
  (appt-message-warning-time 15)
  (appt-disp-window-function #'appt-display-with-notification))

;; EasyPG
(use-package epg-config
  :ensure nil
  :custom
  (epg-pinentry-mode 'loopback))

;; Simple HTML Renderer
(use-package shr
  :ensure nil
  :custom
  (shr-use-fonts nil)
  (shr-use-colors nil)
  (shr-cookie-policy nil)
  (shr-image-animate nil)
  (shr-blocked-images "."))

;; quick access to files/tags
(use-package speedbar
  :ensure nil
  :custom-face
  ;; from lethe theme
  (speedbar-button-face ((t (:foreground "green4"))))
  (speedbar-directory-face ((t (:foreground "blue"))))
  (speedbar-file-face ((t (:foreground "cyan4"))))
  (speedbar-highlight-face ((t (:background "green"))))
  (speedbar-selected-face ((t (:underline t :foreground "red"))))
  (speedbar-tag-face ((t (:foreground "brown"))))
  :custom
  (speedbar-use-images nil)
  (speedbar-show-unknown-files t)
  (speedbar-indentation-width 2))

;; make speedbar same frame
(use-package sr-speedbar
  :ensure t
  :bind ("<f8>" . sr-speedbar-toggle)
  :custom
  (sr-speedbar-default-width 20)
  (sr-speedbar-max-width 30)
  (sr-speedbar-right-side nil)
  (sr-speedbar-skip-other-window-p nil))

;; window layout manager
;;
;; gt next-tab
;; gT prev-tab
(use-package tab-bar
  :ensure nil
  :when (>= emacs-major-version 27)
  :hook (after-init . (lambda ()
                        (tab-bar-mode)
                        (tab-bar-history-mode)))
  :config
  ;; HACK no tab-bar-lines displayed
  (defun no-tab-bar-lines (&rest _)
    (dolist (frame (frame-list))
      (set-frame-parameter frame 'tab-bar-lines 0)))

  (advice-add #'tab-bar-mode :after #'no-tab-bar-lines)
  (advice-add #'make-frame   :after #'no-tab-bar-lines)
  :custom
  (tab-bar-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-close-button-show nil)
  (tab-bar-tab-name-function 'tab-bar-tab-name-all))

(use-package newcomment
  :ensure nil
  :bind ([remap comment-dwim] . #'comment-or-uncomment)
  :config
  (defun comment-or-uncomment (arg)
    "Comment or uncomment the current line or region.

If the prefix ARG is specified, call `comment-indent' on the
current line.
Else, if the region is active and `transient-mark-mode' is on,
call `comment-or-uncomment-region'.
Else, if the current line is empty, insert a comment and indent
it.
Else, call `comment-or-uncomment-region' on the current line."
    (interactive "*P")
    (if arg (comment-indent)
      (if (region-active-p)
          (comment-or-uncomment-region (region-beginning) (region-end))
        (if (save-excursion
              (beginning-of-line)
              (looking-at "\\s-*$"))
            (comment-dwim nil)
          (comment-or-uncomment-region (line-beginning-position) (line-end-position))))))
  :custom
  ;; `auto-fill' inside comments.
  ;;
  ;; The quoted text in `message-mode' are identified as comments, so only
  ;; quoted text can be `auto-fill'ed.
  (comment-auto-fill-only-comments t))

;; transparent remote access
(use-package tramp
  :ensure nil
  :defer t
  :custom
  (tramp-default-method "ssh"))

;; htop like monitor
(use-package proced
  :ensure nil
  :defer t
  :custom
  ;; same with htop
  (proced-auto-update-interval 2)
  (proced-auto-update-flag t))

(use-package calculator
  :ensure nil
  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state 'calculator-mode 'emacs))
  :custom
  (calculator-use-menu nil)
  (calculator-unary-style 'prefix)
  (calculator-user-operators nil))

;; mouse wheel optimization
(use-package mwheel
  :ensure nil
  :defer t
  :custom
  (mouse-wheel-progressive-speed nil)
  (mouse-wheel-scroll-amount '(1 ((shift) . 2) ((control)))))

;; Better abbrev expansion
(use-package hippie-exp
  :ensure nil
  :bind ([remap dabbrev-expand] . hippie-expand)
  :custom
  (hippie-expand-try-functions-list '(try-expand-dabbrev
                                      try-expand-dabbrev-all-buffers
                                      try-expand-dabbrev-from-kill
                                      try-complete-file-name-partially
                                      try-complete-file-name
                                      try-expand-all-abbrevs
                                      try-expand-list
                                      try-expand-line
                                      try-complete-lisp-symbol-partially
                                      try-complete-lisp-symbol))
  )

;; Make align be a simple thing
(use-package align
  :ensure nil
  :bind ("C-c =" . align-regexp))

;; Needed by `webpaste'
(use-package browse-url
  :ensure nil
  :custom
  (browse-url-generic-program (or (executable-find "firefox")
                                  (executable-find "chromium")
                                  (executable-find "google-chrome-stable")
                                  (executable-find "google-chrome")
                                  (when (eq system-type 'darwin) "open")
                                  (when (eq system-type 'gnu/linux) "xdg-open")))
  (browse-url-handlers '(("\\`file:" . browse-url-default-browser))))

;; Buffer manager
(use-package ibuffer
  :ensure nil
  :hook ((ibuffer-mode . ibuffer-auto-mode)
         (ibuffer-mode . (lambda ()
                           (ibuffer-switch-to-saved-filter-groups "Default"))))
  :custom
  (ibuffer-expert t)
  (ibuffer-movement-cycle nil)
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-saved-filter-groups
   '(("Default"
      ("Emacs" (or (name . "\\*scratch\\*")
                   (name . "\\*dashboard\\*")
                   (name . "\\*compilation\\*")
                   (name . "\\*Backtrace\\*")
                   (name . "\\*Packages\\*")
                   (name . "\\*Messages\\*")
                   (name . "\\*Customize\\*")))
      ("News" (or (name . "\\*newsticker\\*")))
      ("Help" (or (name . "\\*Help\\*")
                  (name . "\\*Apropos\\*")
                  (name . "\\*info\\*")))
      ("Repl" (or (mode . gnuplot-comint-mode)
                  (mode . inferior-emacs-lisp-mode)
                  (mode . inferior-python-mode)))
      ("Term" (or (mode . vterm-mode)
                  (mode . term-mode)
                  (mode . shell-mode)
                  (mode . eshell-mode)))
      ("Mail" (or (mode . mail-mode)
                  (mode . message-mode)
                  (mode . gnus-group-mode)
                  (mode . gnus-summary-mode)
                  (mode . gnus-article-mode)
                  (name . "\\*imap log\\*")
                  (name . "\\.newsrc-dribble")))
      ("Conf" (or (mode . yaml-mode)
                  (mode . conf-mode)))
      ("Magit" (or (mode . magit-repolist-mode)
                   (mode . magit-submodule-list-mode)
                   (mode . git-rebase-mode)
                   (derived-mode . magit-section-mode)))
      ("Prog" (derived-mode . prog-mode))
      ("Dired" (mode . dired-mode))
      ("IRC" (or (mode . rcirc-mode)
                 (mode . erc-mode)))
      ("EBrowse" (or (mode . ebrowse-tree-mode)
                     (mode . ebrowse-member-mode)))
      ("Images" (or (mode . image-mode)
                    (mode . image-dired-display-image-mode)
                    (mode . image-dired-thumbnail-mode)))))))

;; Notifications
(use-package notifications
  :ensure nil
  :when (eq system-type 'gnu/linux)
  :commands notify-send
  :config
  (defalias 'notify-send 'notifications-notify))

(use-package notifications
  :ensure nil
  :when (eq system-type 'darwin)
  :commands notify-send
  :config
  (defun notify-send (&rest params)
    "Send notifications via `terminal-notifier'."
    (let ((title (plist-get params :title))
          (body (plist-get params :body)))
      (start-process "terminal-notifier"
                     nil
                     "terminal-notifier"
                     "-group" "Emacs"
                     "-title" title
                     "-message" body
                     "-activate" "org.gnu.Emacs"))))

(use-package notifications
  :ensure nil
  :unless (memq system-type '(gnu/linux darwin))
  :commands notify-send
  :config
  (defalias 'notify-send 'ignore))

;; Recently opened files
(use-package recentf
  :ensure nil
  :defines no-littering-etc-directory no-littering-var-directory
  :after no-littering
  :hook ((after-init . recentf-mode)
         (focus-out-hook . (recentf-save-list recentf-cleanup)))
  :custom
  (recentf-max-saved-items 300)
  (recentf-auto-cleanup 'never)
  (recentf-exclude `(,(expand-file-name package-user-dir)
                     ,quelpa-packages-dir
                     ,no-littering-var-directory
                     ,no-littering-etc-directory
                     ".cache"
                     "cache"
                     "^/tmp/"
                     "/ssh:"
                     "/su\\(do\\)?:"
                     "^/usr/include/"
                     "COMMIT_EDITMSG\\'")))

;; Try out emacs package without installing
(use-package try
  :ensure t
  :defer t)

;; Keep ~/.emacs.d clean
(use-package no-littering
  :ensure t
  :demand t)

;; MacOS specific
(use-package exec-path-from-shell
  :ensure t
  :when (memq window-system '(mac ns x))
  :hook (after-init . exec-path-from-shell-initialize))

(provide 'init-base)

;;; init-base.el ends here
