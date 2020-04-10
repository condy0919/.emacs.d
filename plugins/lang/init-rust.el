;;; init-rust.el --- Rust -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode)
  :custom
  (rust-format-on-save t))

(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :ensure t
  :after rust-mode
  :hook (flycheck-mode . flycheck-rust-setup))

(provide 'init-rust)

;;; init-rust.el ends here
