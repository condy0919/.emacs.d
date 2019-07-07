(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(setq magit-status-margin
  '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))


(provide 'init-git)
