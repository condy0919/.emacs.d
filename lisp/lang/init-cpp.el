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
  (c-comment-prefix-regexp '((c-mode   . "//+!?\\|\\**")
                             (c++-mode . "//+!?\\|\\**")
                             (awk-mode . "#+")
                             (other    . "//+\\|\\**")))
  (c-doc-comment-style `((c-mode   . gtkdoc)
                         ,(if (>= emacs-major-version 28)
                               '(c++-mode . doxygen)
                             '(c++-mode . gtkdoc))))
  (c-offsets-alist '((inline-open           . 0)
                     (brace-list-open       . 0)
                     (inextern-lang         . 0)
                     (statement-case-open   . 4)
                     (statement-cont        . (c-lineup-ternary-bodies +))
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
  :mode (("\\.l\\'" . flex-mode)
         ("\\.y\\'" . bison-mode)))

;; LLVM IR
(use-package llvm-mode
  :ensure t
  :quelpa (llvm-mode :fetcher url
                     :url "https://raw.githubusercontent.com/llvm/llvm-project/master/llvm/utils/emacs/llvm-mode.el")
  :mode ("\\.ll\\'" . llvm-mode))

;; TableGen description
(use-package tablegen-mode
  :ensure t
  :quelpa (tablegen-mode :fetcher url
                         :url "https://raw.githubusercontent.com/llvm/llvm-project/master/llvm/utils/emacs/tablegen-mode.el")
  :mode ("\\.td\\'" . tablegen-mode))

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
  (when (eq system-type 'darwin)
    (add-to-list 'hide-ifdef-env '(__APPLE__ . 1))
    (add-to-list 'hide-ifdef-env '(__clang__ . 1))
    (add-to-list 'hide-ifdef-env '(__llvm__ . 1)))
  :custom
  (hide-ifdef-initially t)
  (hide-ifdef-shadow t))

;; Snippets for C/C++
(use-package tempo
  :ensure nil
  :after cc-mode
  :hook ((c-mode . c-mode-tempo-setup)
         (c++-mode . c++-mode-tempo-setup))
  :bind (:map c++-mode-map
         ("C-c C-e" . tempo-expand-if-complete)
         :map c-mode-map
         ("C-c C-e" . tempo-expand-if-complete))
  :config
  (defvar c-tempo-tags nil)
  (defvar c++-tempo-tags nil)

  (defun c-mode-tempo-setup ()
    (tempo-use-tag-list 'c-tempo-tags))
  (defun c++-mode-tempo-setup ()
    (tempo-use-tag-list 'c-tempo-tags)
    (tempo-use-tag-list 'c++-tempo-tags))

  (tempo-define-template "c-ifndef"
                         '("#ifndef " (P "ifndef-clause: " clause) > n
                           "#define " (s clause) n> p n
                           "#endif // " (s clause) n>
                           )
                         "ifndef"
                         "Insert #ifndef #define #endif directives"
                         'c-tempo-tags)
  (tempo-define-template "c-main"
                         '("int main(int argc, char* argv[]) {" > n>
                           p n
                           "}" > n>
                           )
                         "main"
                         "Insert a main function"
                         'c-tempo-tags)
  (tempo-define-template "c++-class-comparison-operators"
                         '((P "type: " type 'noinsert)
                           (tempo-save-named 'inline (if (y-or-n-p "inline? ") "inline " ""))
                           (tempo-save-named 'constexpr (if (y-or-n-p "constexpr? ") "constexpr " ""))
                           (tempo-save-named 'noexcept (if (y-or-n-p "noexcept? ") "noexcept " ""))
                           (s inline) (s constexpr) "bool operator==(const " (s type) "& lhs, const " (s type) "& rhs) " (s noexcept) "{" > n>
                           p n
                           "}" > n>
                           (s inline) (s constexpr) "bool operator!=(const " (s type) "& lhs, const " (s type) "& rhs) " (s noexcept) "{" > n>
                           "return !(lhs == rhs);" > n>
                           "}" > n>
                           (s inline) (s constexpr) "bool operator<(const " (s type) "& lhs, const " (s type) "& rhs) " (s noexcept) "{" > n>
                           p n
                           "}" > n>
                           (s inline) (s constexpr) "bool operator>(const " (s type) "& lhs, const " (s type) "& rhs) " (s noexcept) "{" > n>
                           "return rhs < lhs;" > n>
                           "}" > n>
                           (s inline) (s constexpr) "bool operator<=(const " (s type) "& lhs, const " (s type) "& rhs) " (s noexcept) "{" > n>
                           "return !(lhs > rhs);" > n>
                           "}" > n>
                           (s inline) (s constexpr) "bool operator>=(const " (s type) "& lhs, const " (s type) "& rhs) " (s noexcept) "{" > n>
                           "return !(lhs < rhs);" > n>
                           "}" > n>
                           )
                         "cmpop"
                         "Insert C++ class comparison operators"
                         'c++-tempo-tags)
  )

;; C++ class browser in Emacs
(use-package ebrowse
  :ensure nil
  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state 'ebrowse-tree-mode 'emacs)
    (evil-set-initial-state 'ebrowse-member-mode 'emacs))
  :custom
  (ebrowse--indentation 2))

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
