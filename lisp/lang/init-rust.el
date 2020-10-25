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

(use-package flycheck-rust
  :ensure t
  :after rust-mode
  :hook (flycheck-mode . flycheck-rust-setup))

;; Snippets for Rust
(use-package tempo
  :ensure nil
  :after rust-mode
  :hook (rust-mode . rust-mode-tempo-setup)
  :config
  (defvar rust-tempo-tags nil)

  (defun rust-mode-tempo-setup ()
    (tempo-use-tag-list 'rust-tempo-tags))

  (tempo-define-template "rust-main"
                         '("fn main() {" > n>
                           p n
                           "}" > n>
                           )
                         "main"
                         "Insert a main function"
                         'rust-tempo-tags)
  (tempo-define-template "rust-testmod"
                         '("#[cfg(test)]" > n>
                           "mod tests {" > n>
                           "use super::*;" > n>
                           p n
                           "}" > n>
                           )
                         "testmod"
                         "Insert a test module"
                         'rust-tempo-tags)
  (tempo-define-template "rust-test"
                         '("#[test]" > n>
                           "fn " (P "test-name: " name) "() {" > n>
                           p n
                           "}" > n>
                           )
                         "test"
                         "Insert a test function"
                         'rust-tempo-tags)
  )

(provide 'init-rust)

;;; init-rust.el ends here
