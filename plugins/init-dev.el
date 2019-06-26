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
