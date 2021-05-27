;;; init-rust.el --- Rust -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package rust-mode
  :ensure t
  :defines lsp-rust-server
  :mode ("\\.rs\\'" . rust-mode)
  :config
  ;; Prefer `rust-analyzer' over `rls'
  (with-eval-after-load 'lsp-mode
    (when (executable-find "rust-analyzer")
      (setq lsp-rust-server 'rust-analyzer)))
  :custom
  (rust-format-on-save (executable-find "rustfmt")))

;; Cargo integration
(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(provide 'init-rust)
;;; init-rust.el ends here
