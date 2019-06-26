(use-package evil
  :ensure t
  :hook (after-init . evil-mode)
  :config
  (setcdr evil-insert-state-map nil)
  :bind (:map evil-motion-state-map
              ("C-u" . scroll-down-command)
              :map evil-insert-state-map
	      ([escape] . evil-normal-state)))

(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "\\")
)

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package treemacs-evil
  :ensure t)


(provide 'init-evil)
