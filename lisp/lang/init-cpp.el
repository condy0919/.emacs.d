;;; init-cpp.el --- Cpp -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(eval-when-compile
  (require 'init-macros))

(use-package cc-mode
  :ensure nil
  :defines lsp-clients-clangd-executable lsp-clients-clangd-args
  :mode ("\\.cxx\\'" . cc-mode)
  :hook (c-mode . (lambda ()
                    (setq comment-start "// "
                          comment-end "")))
  :config
  (setq c-basic-offset 4)

  (defconst ccls-args nil)
  (defconst clangd-args '("-j=2"
                          "--background-index"
                          "--clang-tidy"
                          "--completion-style=bundled"
                          "--pch-storage=memory"
                          "--suggest-missing-includes"
                          "--header-insertion-decorators=0"))
  (with-eval-after-load 'lsp-mode
    ;; Prefer `clangd' over `ccls'
    (cond ((executable-find "clangd") (setq lsp-clients-clangd-executable "clangd"
                                            lsp-clients-clangd-args clangd-args))
          ((executable-find "ccls") (setq lsp-clients-clangd-executable "ccls"
                                          lsp-clients-clangd-args ccls-args))))
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
                     (innamespace           . -) ;; no indentation after namespace
                     (arglist-cont-nonempty . +))))

;; Superb compiler explorer implementation
(use-package rmsbolt
  :ensure t
  :commands rmsbolt-compile
  :defer 1
  :custom
  (rmsbolt-asm-format nil)
  (rmsbolt-default-directory "/tmp"))

;; Parser generator
(use-package bison-mode
  :ensure t
  :mode (("\\.ll\\'" . flex-mode)
         ("\\.yy\\'" . bison-mode)))

;; Highlight "#if 0" as comments
(use-package hideif
  :ensure nil
  :hook ((c-mode c++-mode) . hide-ifdef-mode)
  :config
  ;; org src block doesn't have a corresponding file
  (my/ignore-errors-for hide-ifdef-guts)

  (when (eq system-type 'gnu/linux)
    (add-to-list 'hide-ifdef-env '(__linux__ . 1))
    (add-to-list 'hide-ifdef-env '(__GNUC__ . 11)))
  :custom
  (hide-ifdef-initially t)
  (hide-ifdef-shadow t))

(use-package modern-cpp-font-lock
  :ensure t
  :hook (c++-mode . modern-c++-font-lock-mode)
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
  (modern-c++-literal-dec-suffix-face    'font-lock-constant-face))

(use-package cmake-mode
  :ensure t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'"         . cmake-mode))
  :bind (:map cmake-mode-map
         ;; Compatible with lsp-mode keybindings
         ("C-c d" . cmake-help))
  :config
  (my/set-company-backends-for cmake-mode company-cmake))

(use-package cmake-font-lock
  :ensure t
  :hook (cmake-mode . cmake-font-lock-activate))

(provide 'init-cpp)

;;; init-cpp.el ends here
