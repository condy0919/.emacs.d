;;; init-tools.el --- We all like productive tools

;;; Commentary:
;;

;;; Code:

;; Tips for next keystroke
(use-package which-key
  :ensure t
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
  :bind ("M-s" . avy-goto-word-1))

;; fuzzy matcher
(use-package counsel
  :ensure t
  :defer t
  :diminish counsel-mode
  :hook (ivy-mode . counsel-mode)
  :bind (("M-y" . counsel-yank-pop)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)))

;; buffer
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :init (setq ivy-use-virtual-buffers t
              ivy-count-format "%d/%d"
              ivy-display-style 'fancy)
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-c C-r" . ivy-resume))
  :hook (after-init . ivy-mode))

;; search in local buffer
(use-package swiper
  :ensure t
  :defer t
  :bind (("C-s" . swiper-isearch)))

;; switch windows quickly
(use-package ace-window
  :ensure t
  :defer t
  :bind (:map global-map
         ("M-o" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?h ?j ?k ?l)
        aw-scope 'frame))

(provide 'init-tools)

;;; init-tools.el ends here
