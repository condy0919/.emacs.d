;;; init-rust.el --- Rust -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package rust-mode
  :ensure t
  :defines lsp-rust-server
  :mode ("\\.rs\\'" . rust-mode)
  :config
  ;; Recently `lsp-rust-server' defaults to `rust-analyzer'.
  (with-eval-after-load 'lsp-mode
    (when (executable-find "rls")
      (setq lsp-rust-server 'rls)))
  :custom
  (rust-format-on-save (executable-find "rustfmt")))

;; Cargo integration
(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(provide 'init-rust)
;;; init-rust.el ends here
