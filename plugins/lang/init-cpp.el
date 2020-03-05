;;; init-cpp.el --- Cpp -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package cc-mode
  :ensure nil
  :defines (lsp-clients-clangd-args)
  :bind (:map c++-mode-map
         ("C-c C-o" . ff-find-related-file))
  :config
  (c-set-offset 'innamespace [0]) ;; no indentation after namespace
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (with-eval-after-load 'lsp-mode
    (setq lsp-clients-clangd-args
          '("-j=2"
            "--background-index"
            "--clang-tidy"
            "--completion-style=bundled"
            "--pch-storage=memory"
            "--suggest-missing-includes"
            )))
  )

(use-package modern-cpp-font-lock
  :ensure t
  :diminish t
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package cmake-mode
  :ensure t)

(provide 'init-cpp)

;;; init-cpp.el ends here
