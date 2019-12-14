(use-package evil
  :ensure t
  :hook (after-init . evil-mode)
  :config
  (setcdr evil-insert-state-map nil)
  :bind (:map evil-motion-state-map
              ("C-u" . scroll-down-command)
              :map evil-insert-state-map
          ([escape] . evil-normal-state)))

(use-package evil-surround
  :ensure t
  :hook (prog-mode . evil-surround-mode))

(use-package evil-magit
  :ensure t)

(provide 'init-evil)
