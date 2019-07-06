;; common
;; Compilation Mode
(use-package compile
  :ensure nil
  :preface
  ;; ANSI Coloring
  ;; @see https://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode
  (defun my-colorize-compilation-buffer ()
    "ANSI coloring in compilation buffers."
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :hook (compilation-filter . my-colorize-compilation-buffer))

(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode))

;; 高亮 TODO
(use-package hl-todo
  :ensure t
  :bind (:map hl-todo-mode-map
              ([C-f3] . hl-todo-occur)
              ("C-c t p" . hl-todo-previous)
              ("C-c t n" . hl-todo-next)
              ("C-c t o" . hl-todo-occur))
  :hook (after-init . global-hl-todo-mode)
  :config
  (dolist (keyword '("BUG" "ISSUE" "FIXME" "XXX"))
    (cl-pushnew `(,keyword . ,(face-foreground 'error)) hl-todo-keyword-faces))
  (dolist (keyword '("WORKAROUND" "HACK" "TRICK"))
    (cl-pushnew `(,keyword . ,(face-foreground 'warning)) hl-todo-keyword-faces)))

;; 通过 rg 跳转至定义处
(use-package dumb-jump
  :ensure t
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :hook (after-init . dumb-jump-mode)
  :config
  (setq dumb-jump-prefer-searcher 'rg)
  (with-eval-after-load 'ivy
    (setq dumb-jump-selector 'ivy)))

;; C/C++
(use-package ccls
  :ensure t
  :config
  (setq ccls-sem-highlight-method 'font-lock
	ccls-initialization-options
	`(:clang
	  (:extraArgs ["-std=c++17"]))
    )
  :hook ((c-mode c++-mode objc-mode) .
	 (lambda () (require 'ccls) (lsp)))
)

(use-package cc-mode
  :ensure nil
  :hook (c-mode-common . (lambda ()
                           (c-set-style "stroustrup")
                           (setq tab-width 4
                                 c-basic-offset 4))))

(use-package modern-cpp-font-lock
  :ensure t
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package clang-format
  :ensure t
  :commands (clang-format-region)
  :bind (:map c-mode-base-map
              ("C-c f" . clang-format-region))
)

;; rust
(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t)
  (use-package cargo
    :ensure t
    :diminish cargo-minor-mode
    :hook (rust-mode . cargo-minor-mode)))

(use-package flycheck-rust
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  (add-hook 'rust-mode-hook 'flycheck-mode))

(provide 'init-dev)
