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
  (rust-indent-where-clause t)
  (rust-load-optional-libraries t))

(provide 'init-rust)
;;; init-rust.el ends here
