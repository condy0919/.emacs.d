;;; init-cpp.el --- Cpp -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package cc-mode
  :ensure nil
  :mode ("\\.cxx\\'" . cc-mode)
  :hook (c-mode . (lambda ()
                    (setq comment-start "// "
                          comment-end "")))
  :defines (lsp-clients-clangd-args)
  :custom
  (c-offsets-alist '((inline-open           . 0)
                     (brace-list-open       . 0)
                     (inextern-lang         . 0)
                     (statement-case-open   . 4)
                     (access-label          . -)
                     (case-label            . 0)
                     (member-init-intro     . +)
                     (topmost-intro         . 0)
                     (inlambda              . 0) ;; better indentation for lambda
                     (innamespace           . 0) ;; no indentation after namespace
                     (arglist-cont-nonempty . +)))
  :config
  (setq c-basic-offset 4)
  (with-eval-after-load 'lsp-mode
    (setq lsp-clients-clangd-args
          '("-j=2"
            "--background-index"
            "--clang-tidy"
            "--completion-style=bundled"
            "--pch-storage=memory"
            "--suggest-missing-includes")))
  )

(use-package modern-cpp-font-lock
  :ensure t
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package cmake-mode
  :ensure t
  :defines (company-backends)
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode))
  :config
  (with-eval-after-load 'company-mode
    (add-to-list 'company-backends 'company-cmake)))

(provide 'init-cpp)

;;; init-cpp.el ends here
