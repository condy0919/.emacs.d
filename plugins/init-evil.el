;;; init-evil.el --- Bring vim back -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package evil
  :ensure t
  :diminish evil
  :custom
  (evil-disable-insert-state-bindings t)
  (evil-want-fine-undo t)
  (evil-want-C-u-scroll t)
  (evil-want-Y-yank-to-eol t)
  :hook (after-init . evil-mode)
  :bind (:map evil-normal-state-map
         ("M-." . xref-find-definitions))
  )

(use-package evil-leader
  :ensure t
  :custom (evil-leader/leader "<SPC>")
  :config
  (global-evil-leader-mode))

(use-package evil-nerd-commenter
  :ensure t
  :config
  (global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines))

(use-package evil-surround
  :ensure t
  :hook ((prog-mode markdown-mode conf-mode) . evil-surround-mode))

(use-package evil-magit
  :ensure t
  :after evil magit)

(provide 'init-evil)

;;; init-evil.el ends here
