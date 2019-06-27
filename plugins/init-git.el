(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode t)
  (setq projectile-completion-system 'ivy)
  :bind (("C-c p a" . projectile-find-file)))

(use-package treemacs-projectile
  :ensure t)


(provide 'init-git)
