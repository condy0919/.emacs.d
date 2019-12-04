(setq inhibit-startup-screen t
      inhibit-startup-message t
      make-backup-files nil
      auto-save-default nil
      ring-bell-function 'ignore
      blink-cursor-mode nil
      scroll-conservatively 1000)

(setq-default indent-tabs-mode nil)

(set-face-attribute 'default nil :height 120)

(fset 'yes-or-no-p 'y-or-n-p)

(delete-selection-mode t)
(global-hl-line-mode t)

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

(use-package try
  :ensure t)

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode))

(use-package beacon
  :ensure t
  :config
  (beacon-mode 1))

(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "RET") 'newline-and-indent)

(provide 'init-base)
