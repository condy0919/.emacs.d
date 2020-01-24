;;; init-base.el --- The necessary settings -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; Supress GUI features
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      inhibit-startup-message t)

;; No backup files
(setq make-backup-files nil
      auto-save-default nil)

;; Supress annoying features
(setq ring-bell-function 'ignore
      blink-cursor-mode nil)

;; smooth scroll
(setq scroll-conservatively 1000)

(setq scroll-preserve-screen-position 'always)

;; Newline behaviour
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "S-<return>") 'comment-indent-new-line)

(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")

(setq sentence-end "\\([。、！？]\\|……\\|[,.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

;; No tabs
(setq-default indent-tabs-mode nil)

;; font size
(set-face-attribute 'default nil :height 140)

;; Prefer shorter names
(fset 'yes-or-no-p 'y-or-n-p)

(defalias 'list-buffers 'ibuffer)

;; Keep clean
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

;; Highlight parenthesises
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

;; The selected region of text can be deleted
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; show line/column number
(use-package simple
  :ensure nil
  :hook (after-init . (lambda ()
                        (line-number-mode)
                        (column-number-mode))))

;; back to the previous position
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

;; highlight current line
(use-package hl-line
  :ensure nil
  :hook (after-init . global-hl-line-mode))

;; Try out emacs package without installing
(use-package try
  :ensure t)

(provide 'init-base)

;;; init-base.el ends here
