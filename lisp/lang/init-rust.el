;;; init-rust.el --- Rust -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode)
  :config
  (with-no-warnings
    (with-eval-after-load 'lsp-mode
      (setq lsp-rust-analyzer-diagnostics-disabled ["unresolved-extern-crate"])))
  :custom
  (rust-format-show-buffer nil)
  (rust-format-on-save (executable-find "rustfmt")))

;; Cargo integration
(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(provide 'init-rust)
;;; init-rust.el ends here
