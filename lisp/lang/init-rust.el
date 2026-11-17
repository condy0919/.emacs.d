;;; init-rust.el --- Rust -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode)
  :config
  ;; Keep snake_case whole without Superword's incompatible Evil boundaries.
  (modify-syntax-entry ?_ "w" rust-mode-syntax-table)
  (with-no-warnings
    (with-eval-after-load 'lsp-mode
      (setq lsp-rust-analyzer-diagnostics-disabled ["unresolved-extern-crate"])))
  :custom
  (rust-indent-where-clause t)
  (rust-load-optional-libraries t))

(provide 'init-rust)
;;; init-rust.el ends here
