;; 快速搜索
(use-package ripgrep
  :ensure t)

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

;; lint 工具
(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode)
  :diminish " FC")

(provide 'init-misc)
