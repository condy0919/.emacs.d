;;; init-tools.el --- We all like productive tools -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; Tips for next keystroke
(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode)
  :custom (which-key-idle-delay 0.5))

;; The blazing grep tool
(use-package rg
  :ensure t
  :defer t)

;; Fuzzy search
(use-package fzf
  :ensure t
  :defer t)

;; Jump to arbitrary positions
(use-package avy
  :ensure t
  :custom
  (avy-timeout-seconds 0.2)
  (avy-all-windows nil)
  (avy-background t)
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?q ?w ?e ?r ?u ?i ?o ?p)))

;; ivy core
(use-package ivy
  :ensure t
  :hook (after-init . ivy-mode)
  :bind (("C-c C-r" . ivy-resume)
         :map ivy-minibuffer-map
         ("C-c C-e" . my/ivy-woccur)
         :map ivy-occur-mode-map
         ("C-c C-e" . ivy-wgrep-change-to-wgrep-mode)
         :map ivy-occur-grep-mode-map
         ("C-c C-e" . ivy-wgrep-change-to-wgrep-mode))
  :custom
  (ivy-display-style 'fancy)          ;; fancy style
  (ivy-count-format "%d/%d ")         ;; better counts
  (ivy-use-virtual-buffers t)         ;; show recent files
  (ivy-on-del-error-function 'ignore) ;; dont quit minibuffer when del-error
  :preface
  ;; Copy from
  ;; https://github.com/honmaple/maple-emacs/blob/master/lisp/init-ivy.el
  (defun my/ivy-woccur ()
    "ivy-occur with wgrep-mode enabled."
    (interactive)
    (run-with-idle-timer 0 nil 'ivy-wgrep-change-to-wgrep-mode)
    (ivy-occur))
  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state 'ivy-occur-grep-mode 'normal)
    (evil-make-overriding-map ivy-occur-mode-map 'normal)))

;; Fuzzy matcher
(use-package counsel
  :ensure t
  :hook (ivy-mode . counsel-mode)
  :bind (([remap evil-ex-registers]  . counsel-evil-registers)
         ([remap evil-show-mark]     . counsel-mark-ring)
         ([remap recentf-open-files] . counsel-recentf)
         ([remap swiper]             . counsel-grep-or-swiper)
         ("M-y"                      . counsel-yank-pop))
  :preface
  (defun my/rename-file (file)
    "Rename `FILE'. If the `FILE' is opened, kill the buffer too."
    (interactive)
    (let* ((new-name (read-string "NewName: "))
           (old-dir (file-name-directory file))
           (new-file (concat old-dir new-name)))
      (rename-file file new-file)
      (when-let* ((buf (find-buffer-visiting file)))
        (kill-buffer buf)
        (find-file new-file))))
  (defun my/delete-file (file)
    "Delete `FILE'. If the `FILE' is opened, kill the buffer too."
    (interactive)
    (when (y-or-n-p (format "Really delete '%s'? " file))
      (delete-file file)
      (when-let* ((buf (find-buffer-visiting file)))
        (kill-buffer buf))))
  :config
  (ivy-set-actions
   'counsel-find-file
   '(("d" my/delete-file "delete")
     ("r" my/rename-file "rename")
     ("x" counsel-find-file-as-root "open as root")))
  :custom
  (counsel-preselect-current-file t)
  (counsel-yank-pop-preselect-last t)
  (counsel-yank-pop-separator "\n-----------\n")
  (counsel-find-file-at-point t)
  (counsel-find-file-ignore-regexp "\\(?:\\`\\(?:\\.\\|__\\)\\|elc\\|pyc$\\)"))

;; Use swiper less, it takes up `ivy-height' lines.
(use-package isearch
  :ensure nil
  :bind (:map isearch-mode-map
         ;; consistent with ivy-occur
         ("C-c C-o" . isearch-occur)
         ;; Edit the search string instead of jumping back
         ([remap isearch-delete-char] . isearch-del-char))
  :custom
  (isearch-lazy-count t)
  (lazy-count-prefix-format "%s/%s ")
  (lazy-count-suffix-format nil)
  (lazy-highlight-cleanup nil))

;; isearch alternative
(use-package swiper
  :ensure t
  :config
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "/" 'swiper)
    )
  )

;; Writable grep buffer. company well with ivy-occur
(use-package wgrep
  :ensure t
  :defer 1
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

;; Switch windows quickly
(use-package ace-window
  :ensure t
  :preface
  (defun my/switch-window ()
    (interactive)
    (if (<= (count-windows) 2)
        (other-window 1)
      (ace-window 1)))
  :bind ("M-o" . my/switch-window)
  :custom-face
  (aw-leading-char-face ((nil (:foreground "deep sky blue" :weight bold :height 3.0))))
  (aw-mode-line-face ((nil (:foreground "lawn green" :inherit mode-line-buffer-id))))
  :custom
  (aw-keys '(?a ?s ?d ?f ?h ?j ?k ?l))
  (aw-scope 'frame))

;; The markdown mode is awesome! unbeatable
(use-package markdown-mode
  :ensure t
  :hook (markdown-mode . auto-fill-mode)
  :custom
  (markdown-command "pandoc")
  (markdown-fontify-code-blocks-natively t)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

;; Free hands
(use-package auto-package-update
  :ensure t
  :defer t
  :custom
  (auto-package-update-delete-old-versions t))

;; Beautiful term mode & friends
(use-package vterm
  :ensure t
  :defines (evil-insert-state-cursor)
  :commands (evil-insert-state vterm)
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-clear-scrollback t)
  :hook (vterm-mode . (lambda ()
                        (setq-local evil-insert-state-cursor 'box)
                        (setq-local global-hl-line-mode nil)
                        ;; Dont prompt about processes when killing vterm
                        (setq confirm-kill-processes nil)
                        (evil-insert-state)))
  )

(use-package shell-pop
  :ensure t
  :bind ("M-=" . shell-pop)
  :custom
  (shell-pop-full-span t)
  (shell-pop-shell-type '("vterm" "*vterm*" #'vterm)))

;; GC optimization
(use-package gcmh
  :ensure t
  :custom
  (gcmh-idle-delay 10)
  (gcmh-high-cons-threshold #x6400000) ;; 100 MB
  :hook (after-init . gcmh-mode))

;; Write documentation comment in an easy way
(use-package separedit
  :ensure t
  :custom
  (separedit-default-mode 'markdown-mode)
  (separedit-remove-trailing-spaces-in-comment t)
  (separedit-continue-fill-column t)
  (separedit-buffer-creation-hook #'auto-fill-mode)
  :bind (:map prog-mode-map
         ("C-c '" . separedit)))

;; Pastebin service
(use-package webpaste
  :ensure t
  :defer 1
  :custom
  (webpaste-open-in-browser t)
  (webpaste-paste-confirmation t)
  (webpaste-add-to-killring nil)
  (webpaste-provider-priority '("paste.mozilla.org" "dpaste.org" "ix.io")))

;; Edit text for browser with GhostText or AtomicChrome extension
(use-package atomic-chrome
  :ensure t
  :commands (evil-set-initial-state)
  :hook ((emacs-startup . atomic-chrome-start-server)
         (atomic-chrome-edit-mode . delete-other-windows))
  :custom
  (atomic-chrome-buffer-open-style 'frame)
  (atomic-chrome-default-major-mode 'markdown-mode)
  :config
  (if (fboundp 'gfm-mode)
      (setq atomic-chrome-url-major-mode-alist
            '(("github\\.com" . gfm-mode))))
  ;; The browser is in "insert" state, makes it consistent
  (evil-set-initial-state 'atomic-chrome-edit-mode 'insert))

;; Open very large files
(use-package vlf
  :ensure t
  :defer t
  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state 'vlf-mode 'normal)
    (evil-define-key 'normal vlf-mode-map
      ;; view
      "]]" 'vlf-next-batch
      "[[" 'vlf-prev-batch

      ;; jump
      "gg" 'vlf-beginning-of-file
      "G"  'vlf-end-of-file))
  )

;; Notes manager
(use-package deft
  :ensure t
  :defer t
  :init
  (setq deft-default-extension "org")
  :custom
  (deft-directory (expand-file-name "~/.deft/"))
  (deft-use-filename-as-title nil)
  (deft-use-filter-string-for-filename t)
  (deft-file-naming-rules '((noslash . "-")
                            (nospace . "-")
                            (case-fn . downcase)))
  :config
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key-for-mode 'deft-mode
      "c" 'deft-filter-clear
      "d" 'deft-delete-file
      "i" 'deft-toggle-incremental-search
      "n" 'deft-new-file
      "N" 'deft-new-file-named
      "q" 'quit-window
      "o" 'deft-open-file-other-window
      "r" 'deft-rename-file))
  (with-eval-after-load 'evil
    (evil-set-initial-state 'deft-mode 'insert)
    (evil-define-key 'normal deft-mode-map
      "gr"        'deft-refresh
      (kbd "C-s") 'deft-filter
      "r"         'deft-rename-file
      "a"         'deft-new-file
      "A"         'deft-new-file-named
      "d"         'deft-delete-file
      "D"         'deft-archive-file
      "q"         'kill-current-buffer))
  )

;; Visual bookmarks
(use-package bm
  :ensure t
  :hook ((after-init   . bm-repository-load)
         (find-file    . bm-buffer-restore)
         (after-revert . bm-buffer-restore)
         (kill-buffer  . bm-buffer-save)
         (kill-emacs   . (lambda ()
                           (bm-buffer-save-all)
                           (bm-repository-save))))
  :custom
  (bm-annotate-on-create t)
  (bm-buffer-persistence t)
  (bm-cycle-all-buffers t)
  (bm-goto-position nil)
  (bm-in-lifo-order t)
  (bm-recenter t))

;; Customize popwin behavior
(use-package shackle
  :ensure t
  :hook (after-init . shackle-mode)
  :custom
  (shackle-default-size 0.33)
  (shackle-default-alignment 'below)
  (shackle-rules '((magit-status-mode    :select t :inhibit-window-quit t :same t)
                   (magit-log-mode       :select t :inhibit-window-quit t :same t)
                   (profiler-report-mode :select t :size 0.5)
                   (help-mode            :select t :align 5 :size 0.4)
                   (comint-mode          :select t :align t :size 0.4)
                   (Man-mode             :select t :other t)
                   (woman-mode           :select t :other t)
                   (grep-mode            :select t :align t)
                   (rg-mode              :select t :align t)
                   ("*bm-bookmarks*"         :select t   :align t)
                   ("*Flycheck errors*"      :select t   :align t)
                   ("*quickrun*"             :select nil :align t :size 15)
                   ("*Backtrace*"            :select t   :align t :size 15)
                   ("*Shell Command Output*" :select nil :align t :size 0.4)
                   ("*Async Shell Command*"  :ignore t)
                   ("\\*ivy-occur .*\\*"     :regexp t :select t :align t)))
  )

(provide 'init-tools)

;;; init-tools.el ends here
