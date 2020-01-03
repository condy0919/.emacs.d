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

(provide 'init-tools)

;;; init-tools.el ends here
