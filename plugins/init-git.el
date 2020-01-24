;;; init-git.el --- Git is awesome -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; The awesome git client
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-status-margin
        '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18)))


;; highlight uncommitted changes using git
(use-package diff-hl
  :ensure t
  :hook ((prog-mode . (lambda ()
                        (diff-hl-mode)
                        (diff-hl-flydiff-mode)
                        (diff-hl-margin-mode)))
         (magit-post-refresh . diff-hl-magit-post-refresh)))

(provide 'init-git)

;;; init-git.el ends here
