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

;; See https://github.com/quelpa/quelpa/issues/205
;;
;; LLVM IR
;; (use-package llvm-mode
;;   :ensure t
;;   :quelpa (llvm-mode :fetcher url
;;                      :url "https://raw.githubusercontent.com/llvm/llvm-project/master/llvm/utils/emacs/llvm-mode.el")
;;   :mode ("\\.ll\\'" . llvm-mode))

;; TableGen description
;; (use-package tablegen-mode
;;   :ensure t
;;   :quelpa (tablegen-mode :fetcher url
;;                          :url "https://raw.githubusercontent.com/llvm/llvm-project/master/llvm/utils/emacs/tablegen-mode.el")
;;   :mode ("\\.td\\'" . tablegen-mode))

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
         ([tab] . expand-or-indent)
         :map c-mode-map
         ([tab] . expand-or-indent))
  :config
  (defun expand-or-indent (&optional arg)
    "Expand tempo or indent current line."
    (interactive "P")
    (unless (tempo-expand-if-complete)
      (indent-for-tab-command arg)))

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

(use-package ebrowse
  :ensure nil
  :hook (ebrowse-tree-mode . ebrowse-toggle-file-name-display)
  :config
  (with-eval-after-load 'evil-collection
    (evil-set-initial-state 'ebrowse-mode 'normal)
    (evil-collection-define-key 'normal 'ebrowse-tree-mode-map
      ;; view
      (kbd "TAB") 'ebrowse-pop/switch-to-member-buffer-for-same-tree
      (kbd "RET") 'ebrowse-find-class-declaration
      (kbd "SPC") 'ebrowse-view-class-declaration
      (kbd "C-l") 'ebrowse-redraw-tree

      "J" 'ebrowse-read-class-name-and-go
      "D" 'ebrowse-remove-class-at-point

      ;; mark
      "m" 'ebrowse-toggle-mark-at-point
      "M" 'ebrowse-mark-all-classes

      ;; show
      "L" 'ebrowse-tree-show/body
      "s" 'ebrowse-statistics

      ;; fold
      "zr" 'ebrowse-expand-all
      "zo" 'ebrowse-expand-branch
      "zc" 'ebrowse-collapse-branch

      ;; quit
      "q" 'bury-buffer)

    (evil-set-initial-state 'ebrowse-member-mode 'normal)
    (evil-collection-define-key 'normal 'ebrowse-member-mode-map
      ;; view
      (kbd "TAB") 'ebrowse-pop-from-member-to-tree-buffer
      (kbd "RET") 'ebrowse-find-member-definition
      (kbd "SPC") 'ebrowse-view-member-definition
      (kbd "C-l") 'ebrowse-redisplay-member-buffer

      "J" 'ebrowse-goto-visible-member/all-member-lists
      "C" 'ebrowse-member-switch/body

      ;; show
      "L" 'ebrowse-member-display/body
      "F" 'ebrowse-member-filter/body
      "T" 'ebrowse-member-toggle/body

      ;; quit
      "q" 'bury-buffer))

  (with-eval-after-load 'hydra
    (defhydra ebrowse-tree-show (:color blue)
      "Show"
      ("F" ebrowse-tree-command:show-static-member-functions "static member functions")
      ("V" ebrowse-tree-command:show-static-member-variables "static member variables")
      ("d" ebrowse-tree-command:show-friends "friend classes & functions")
      ("f" ebrowse-tree-command:show-member-functions "member functions")
      ("t" ebrowse-tree-command:show-types "types")
      ("v" ebrowse-tree-command:show-member-variables "member variables"))

    (defhydra ebrowse-member-switch (:color blue)
      "Switch"
      ("b" ebrowse-switch-member-buffer-to-base-class "base class")
      ("c" ebrowse-switch-member-buffer-to-any-class "any class")
      ("d" ebrowse-switch-member-buffer-to-derived-class "derived class")
      ("n" ebrowse-switch-member-buffer-to-next-sibling-class "next sibling class")
      ("p" ebrowse-switch-member-buffer-to-previous-sibling-class "previous sibling class"))
    (defhydra ebrowse-member-display (:color blue)
      "Display"
      ("F" ebrowse-display-static-functions-member-list "static member functions")
      ("V" ebrowse-display-static-variables-member-list "static member variables")
      ("d" ebrowse-display-friends-member-list "friend classes & functions")
      ("f" ebrowse-display-function-member-list "member functions")
      ("t" ebrowse-display-types-member-list "types")
      ("v" ebrowse-display-variables-member-list "member variables"))
    (defhydra ebrowse-member-filter (:color blue)
      "Filter"
      ("c" ebrowse-toggle-const-member-filter "const")
      ("i" ebrowse-toggle-inline-member-filter "inline")
      ("p" ebrowse-toggle-pure-member-filter "pure")
      ("a" ebrowse-remove-all-member-filters "all")
      ("v" ebrowse-toggle-virtual-member-filter "virtual")
      ("r" ebrowse-toggle-private-member-filter "private")
      ("o" ebrowse-toggle-protected-member-filter "protected")
      ("u" ebrowse-toggle-public-member-filter "public"))
    (defhydra ebrowse-member-toggle (:color blue)
      "Toggle"
      ("a" ebrowse-toggle-member-attributes-display "attributes")
      ("b" ebrowse-toggle-base-class-display "base class")
      ("l" ebrowse-toggle-long-short-display "long display")))
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
