;;; init-tools.el --- We all like productive tools -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; Tips for next keystroke
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

;; The blazing grep tool
(use-package rg
  :ensure t
  :defer t)

;; fuzzy search
(use-package fzf
  :ensure t
  :defer t)

;; Jump to arbitrary positions
(use-package avy
  :ensure t
  :config
  ;; evil-leader keybindings
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "w" 'avy-goto-word-0
      "l" 'avy-goto-line))
  )

;; fuzzy matcher
(use-package counsel
  :ensure t
  :diminish counsel-mode
  :hook (ivy-mode . counsel-mode)
  :bind (("M-y" . counsel-yank-pop)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x b" . counsel-ibuffer)))

;; buffer
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :init (setq ivy-use-virtual-buffers t
              ivy-count-format "%d/%d"
              ivy-display-style 'fancy)
  :bind (("C-c C-r" . ivy-resume))
  :hook (after-init . ivy-mode))

;; search in local buffer
(use-package swiper
  :ensure t
  :bind (("C-s" . swiper-isearch)))

;; switch windows quickly
(use-package ace-window
  :ensure t
  :bind (:map global-map
         ("M-o" . ace-window))
  :config
  (set-face-attribute
   'aw-leading-char-face nil
   :foreground "deep sky blue"
   :weight 'bold
   :height 3.0)
  (set-face-attribute
   'aw-mode-line-face nil
   :inherit 'mode-line-buffer-id
   :foreground "lawn green")
  (setq aw-keys '(?a ?s ?d ?f ?h ?j ?k ?l)
        aw-scope 'frame
        aw-dispatch-always t
        aw-dispatch-alist '((?x aw-delete-window "Ace - Delete Window")
                            (?c aw-swap-window "Ace - Swap Window")
                            (?n aw-flip-window)
                            (?v aw-split-window-vert "Ace - Split Vert Window")
                            (?h aw-split-window-horz "Ace - Split Horz Window")
                            (?m delete-other-windows "Ace - Maximize Window")
                            (?g delete-other-windows)
                            (?b balance-windows)
                            (?u (lambda ()
                                  (progn
                                    (winner-undo)
                                    (setq this-command 'winner-undo))))
                            (?r winner-redo))))

;; The markdown mode is awesome! unbeatable
(use-package markdown-mode
  :ensure t
  :custom
  (markdown-command "pandoc")
  (markdown-fontify-code-blocks-natively t)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :preface
  (defun my/markdown-insert-ruby-tag ()
    (interactive)
    (let ((text (read-string "text: "))
          (extra (read-string "extra text: ")))
      (insert (format "<ruby>%s<rp>(</rp><rt>%s</rt><rp>)</rp></ruby>" text extra))))
  :bind (:map markdown-mode-map
         ("C-c r" . my/markdown-insert-ruby-tag)))

;; free hands
(use-package auto-package-update
  :ensure t
  :custom
  (auto-package-update-delete-old-version t))

;; toggle shell
(use-package aweshell
  :ensure t
  :straight (:host github :repo "manateelazycat/aweshell")
  :bind (("M-=" . aweshell-dedicated-toggle)))

;; GC optimization
(use-package gcmh
  :ensure t
  :custom (gcmh-high-cons-threshold 100000000)
  :hook (after-init . gcmh-mode))

;; required by `comment-edit'
(use-package dash
  :ensure t)

;; required by `comment-edit'
(use-package edit-indirect
  :ensure t)

;; write documentation comment with in a easy way
(use-package comment-edit
  :ensure t
  :straight (:host github :repo "twlz0ne/comment-edit.el")
  :custom
  (comment-edit-default-mode 'markdown-mode)
  (comment-edit-remove-trailing-spaces-in-comment t)
  :bind (:map prog-mode-map
          ("C-c '" . comment-edit)))

;; pastebin service
(use-package webpaste
  :ensure t
  :custom
  (webpaste-paste-confirmation t)
  (webpaste-add-to-killring nil)
  (webpaste-provider-priority '("dpaste.org" "dpaste.com" "ix.io"))
  :config
  (add-hook 'webpaste-return-url-hook
            (lambda (url)
              (message "Opened URL in browser: %s" url)
              (browse-url url)))
  )

(provide 'init-tools)

;;; init-tools.el ends here
