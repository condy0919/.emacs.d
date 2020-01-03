;;; init-base.el --- The necessary settings

;;; Commentary:
;;

;;; Code:

(setq inhibit-startup-screen t
      inhibit-startup-message t
      make-backup-files nil
      auto-save-default nil
      ring-bell-function 'ignore
      blink-cursor-mode nil
      scroll-conservatively 1000)

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "S-<return>") 'comment-indent-new-line)

;; No tabs
(setq-default indent-tabs-mode nil)

;; font size
(set-face-attribute 'default nil :height 140)

(fset 'yes-or-no-p 'y-or-n-p)

(defalias 'list-buffers 'ibuffer)

(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :config (setq show-paren-when-point-inside-paren t
                show-paren-when-point-in-periphery t))

;; The selected region of text can be deleted
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; show line/column number
(use-package simple
  :ensure nil
  :init
  (setq line-number-mode t
        column-number-mode t))

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
