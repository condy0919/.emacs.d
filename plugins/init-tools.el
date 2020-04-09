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
    (interactive)
    (let* ((new-name (read-string "NewName: "))
           (old-dir (file-name-directory file)))
      (rename-file file (concat old-dir new-name))))
  :config
  (ivy-set-actions
   'counsel-find-file
   '(("d" delete-file "delete")
     ("r" my/rename-file "rename")
     ("x" counsel-find-file-as-root "open as root")))
  :custom
  (counsel-preselect-current-file t)
  (counsel-yank-pop-preselect-last t)
  (counsel-yank-pop-separator "\n-----------\n")
  (counsel-find-file-at-point t)
  (counsel-find-file-ignore-regexp "\\(?:\\`\\(?:\\.\\|__\\)\\|elc\\|pyc$\\)"))

;; Dont use swiper
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
  (aw-scope 'frame)
  (aw-dispatch-always t)
  (aw-dispatch-alist '((?x aw-delete-window "Ace - Delete Window")
                       (?c aw-swap-window "Ace - Swap Window")
                       (?n aw-flip-window "Ace - Flip Window")
                       (?v aw-split-window-vert "Ace - Split Vert Window")
                       (?h aw-split-window-horz "Ace - Split Horz Window")
                       (?m delete-other-windows "Ace - Maximize Window")
                       (?g delete-other-windows)
                       (?b balance-windows)
                       (?u (lambda ()
                             (progn
                               (winner-undo)
                               (setq this-command 'winner-undo))))
                       (?r winner-redo)))
  )

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
  :commands (evil-insert-state)
  :custom
  (vterm-module-cmake-args (concat "-DCMAKE_BUILD_TYPE=Release"
                                   (if (executable-find "vterm-ctrl")
                                       " -DUSE_SYSTEM_LIBVTERM=On"
                                     "")))
  (vterm-kill-buffer-on-exit t)
  (vterm-clear-scrollback t)
  :hook (vterm-mode . (lambda ()
                        (setq-local evil-insert-state-cursor 'box)
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
  (gcmh-low-cons-threshold #x10000000)
  (gcmh-high-cons-threshold #x40000000)
  (gcmh-idle-delay 300)
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

(provide 'init-tools)

;;; init-tools.el ends here
