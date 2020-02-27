;;; init-rust.el --- Rust -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package rust-mode
  :ensure t
  :init (setq rust-format-on-save t))

(use-package cargo
  :ensure t
  :diminish cargo-minor-mode
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :ensure t
  :hook (flycheck-mode . flycheck-rust-setup))

(provide 'init-rust)

;;; init-rust.el ends here
