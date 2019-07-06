(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(setq magit-status-margin
  '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode t)
  (setq projectile-completion-system 'ivy)
  :bind (("C-c p a" . projectile-find-file)))

(use-package treemacs-projectile
  :defer t
  :ensure t
  :config
  (setq treemacs-header-function #'treemacs-projectile-create-header)
)


(provide 'init-git)
