(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(setq magit-status-margin
  '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))

;; 显示当前版本与上个版本的差异
(use-package diff-hl
  :ensure t
  :hook (prog-mode . (lambda ()
                       (progn
                         (diff-hl-mode)
                         (diff-hl-flydiff-mode)
                         (diff-hl-margin-mode)))))

(provide 'init-git)
