;;; init-cpp.el --- Cpp -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'init-macros)

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
            "--suggest-missing-includes"
            "--header-insertion-decorators=0")))
  )

(use-package bison-mode
  :ensure t
  :mode (("\\.ll\\'" . flex-mode)
         ("\\.yy\\'" . bison-mode)))

;; Highlight "#if 0" as comments
(use-package hideif
  :ensure nil
  :hook (c-mode-common . hide-ifdef-mode)
  :custom
  (hide-ifdef-initially t)
  (hide-ifdef-shadow t))

(use-package modern-cpp-font-lock
  :ensure t
  :custom
  ;; Make integer literal highlight in the same color
  (modern-c++-literal-binary-prefix-face 'font-lock-constant-face)
  (modern-c++-literal-binary-infix-face  'font-lock-constant-face)
  (modern-c++-literal-binary-suffix-face 'font-lock-constant-face)
  (modern-c++-literal-octal-prefix-face  'font-lock-constant-face)
  (modern-c++-literal-octal-infix-face   'font-lock-constant-face)
  (modern-c++-literal-octal-suffix-face  'font-lock-constant-face)
  (modern-c++-literal-hex-prefix-face    'font-lock-constant-face)
  (modern-c++-literal-hex-infix-face     'font-lock-constant-face)
  (modern-c++-literal-hex-suffix-face    'font-lock-constant-face)
  (modern-c++-literal-dec-infix-face     'font-lock-constant-face)
  (modern-c++-literal-dec-suffix-face    'font-lock-constant-face)
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package cmake-mode
  :ensure t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'"         . cmake-mode))
  :config
  (my/set-company-backends-for cmake-mode company-cmake))

(use-package cmake-font-lock
  :ensure t
  :hook (cmake-mode . cmake-font-lock-activate))

(provide 'init-cpp)

;;; init-cpp.el ends here
