;; 快速搜索
(use-package ripgrep
  :ensure t)

;; 模糊匹配文件
(use-package fzf
  :ensure t)

;; 目录过滤
(use-package dired-narrow
  :ensure t
  :config
  (bind-key "C-c C-n" #'dired-narrow)
  (bind-key "C-c C-f" #'dired-narrow-fuzzy))

;; 显示当前目录下内容
(use-package dired-subtree
  :ensure t
  :after dired
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))

;; 定位单词位置
(use-package avy
  :ensure t
  :bind ("M-s" . avy-goto-word-1))

;; 打开目录
(use-package treemacs
  :ensure t
  :defer t
  :config
  (progn
    (setq treemacs-follow-after-init          t
	  treemacs-width                      35
	  treemacs-indentation                2
	  treemacs-git-integration            t
	  treemacs-collapse-dirs              3
	  treemacs-silent-refresh             nil
	  treemacs-change-root-without-asking nil
	  treemacs-sorting                    'alphabetic-desc
	  treemacs-show-hidden-files          t
	  treemacs-never-persist              nil
	  treemacs-is-never-other-window      nil
	  treemacs-goto-tag-strategy          'refetch-index)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t))
  :bind
  (:map global-map
	([f8]        . treemacs)
	("M-0"       . treemacs-select-window)
	("C-c 1"     . treemacs-delete-other-windows))
)

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

(provide 'init-misc)
