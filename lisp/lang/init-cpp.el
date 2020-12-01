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
                          "--recovery-ast"
                          "--cross-file-rename"
                          "--completion-style=bundled"
                          "--pch-storage=memory"
                          "--suggest-missing-includes"
                          "--header-insertion=iwyu"
                          "--header-insertion-decorators"))
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

  (tempo-define-template "c++-class-equality"
                         '((P "type: " type 'noinsert)
                           (tempo-save-named 'inline (if (y-or-n-p "Inline? ") "inline " ""))
                           (tempo-save-named 'constexpr (if (y-or-n-p "Constexpr? ") "constexpr " ""))
                           (tempo-save-named 'noexcept (if (y-or-n-p "Noexcept? ") "noexcept " ""))
                           (s inline) (s constexpr) "bool operator==(const " (s type) "& lhs, const " (s type) "& rhs) " (s noexcept) "{" > n>
                           p n
                           "}" > n>
                           (s inline) (s constexpr) "bool operator!=(const " (s type) "& lhs, const " (s type) "& rhs) " (s noexcept) "{" > n>
                           "return !(lhs == rhs);" > n>
                           "}" > n>
                           )
                         "eq"
                         "Class equality comparison"
                         'c++-tempo-tags)
  (tempo-define-template "c++-class-totally-ordered"
                         '((P "type: " type 'noinsert)
                           (tempo-save-named 'inline (if (y-or-n-p "Inline? ") "inline " ""))
                           (tempo-save-named 'constexpr (if (y-or-n-p "Constexpr? ") "constexpr " ""))
                           (tempo-save-named 'noexcept (if (y-or-n-p "Noexcept? ") "noexcept " ""))
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
                         "ord"
                         "Class totally ordered comparison"
                         'c++-tempo-tags)
  (tempo-define-template "c++-test-suite"
                         '("BOOST_AUTO_TEST_SUITE(" (P "test-suite: ") ")" > n>
                           p n
                           "BOOST_AUTO_TEST_SUITE_END()" > n>
                           )
                         "ts"
                         "Test suite for C++"
                         'c++-tempo-tags)
  (tempo-define-template "c++-test-case"
                         '("BOOST_AUTO_TEST_CASE(" (P "test-case: ") ") {" > n>
                           p n
                           "}" > n>
                           )
                         "tc"
                         "Test case for C++"
                         'c++-tempo-tags)
  (tempo-define-template "c++-fixture"
                         '("BOOST_FIXTURE_TEST_SUITE(" (P "name: ") ", " (P "fixture: ") ")" > n>
                           p n
                           "BOOST_AUTO_TEST_SUITE_END()" > n>
                           )
                         "fixt"
                         "Fixture for C++"
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

;; cmake, the de factor build system for C++
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


;; Snippets for cmake
(use-package tempo
  :ensure nil
  :after cmake-mode
  :hook (cmake-mode . cmake-mode-tempo-setup)
  :config
  (defvar cmake-tempo-tags nil)

  (defun cmake-mode-tempo-setup ()
    (tempo-use-tag-list 'cmake-tempo-tags))

  (defun proj-prefixed (arg)
    (tempo-process-and-insert-string (concat (upcase (tempo-lookup-named 'proj)) arg)))

  ;; Make sure you have the following directory hierarchy
  ;;
  ;; .
  ;; ├── CMakeLists.txt
  ;; ├── include
  ;; └── src
  ;;     └── lib.cpp
  ;;
  ;; # Features
  ;;
  ;; - Option to use libc++
  ;; - Tag based version
  (tempo-define-template "cmake-library"
                         '((P "project: " proj 'noinsert)
                           "cmake_minimum_required(VERSION 3.11)" n n
                           "include(FetchContent)" n n
                           "project(" (s proj) n
                           "  VERSION 0.1.0" n
                           "  LANGUAGES CXX" n
                           "  DESCRIPTION \"\"" n
                           "  HOMEPAGE_URL \"\"" n
                           ")" n n
                           "OPTION(" (proj-prefixed "_USE_LIBCPP") " \"Use the libc++ library\" OFF)" n n
                           "# Sources for the library are specified at the end" n
                           "add_library(" (s proj) " \"\")" n n
                           "### Commpile options" n n
                           "# Enable C++17 (Required)" n
                           "target_compile_features(" (s proj) n
                           "  PUBLIC" n
                           "    cxx_std_17" n
                           ")" n n
                           "# Common GCC/Clang options" n
                           "target_compile_options(" (s proj) n
                           "  PRIVATE" n
                           "    -Wall" n
                           "    -Wextra" n
                           ")" n n
                           "### Libraries" n n
                           "# Enable threading support" n
                           "set(THREADS_PREFER_PTHREAD_FLAG ON)" n
                           "find_package(Threads REQUIRED)" n
                           "target_link_libraries(" (s proj) " PRIVATE Threads::Threads)" n n
                           "# Link libc++" n
                           "if(" (proj-prefixed "_USE_LIBCPP)") n
                           "  target_compile_options(" (s proj) " PRIVATE -stdlib=libc++)" n
                           "  target_link_libraries(" (s proj) " PRIVATE c++ c++abi)" n
                           "endif()" n n
                           "### Definitions" n n
                           "### Includes" n n
                           "target_include_directories(" (s proj) " PRIVATE include)" n n
                           "### Install" n n
                           "install(TARGETS " (s proj) " RUNTIME DESTINATION bin)" n n
                           "### Sources" n n
                           "target_sources(" (s proj) n
                           "  PRIVATE" n
                           "    src/lib.cpp" n
                           ")" n n
                           "### Obtain version information from Git" n n
                           "# if(NOT " (proj-prefixed "_VERSION)") n
                           "#   execute_process(COMMAND git describe --tag --long HEAD" n
                           "#     OUTPUT_VARIABLE " (proj-prefixed "_VERSION") n
                           "#     OUTPUT_STRIP_TRAILING_WHITESPACE" n
                           "#     WORKING_DIRECTORY \"${CMAKE_CURRENT_SOURCE_DIR}\")" n n
                           "#   if(NOT " (proj-prefixed "_VERSION)") n
                           "#     set(" (proj-prefixed "_VERSION") " \"<unknown>\")" n
                           "#   endif()" n
                           "# endif()" n n
                           "# set_property(SOURCE src/main.cpp APPEND PROPERTY" n
                           "#              COMPILE_DEFINITIONS VERSION=\\\"${" (proj-prefixed "_VERSION") "}\\\")" n
                           )
                         "lib"
                         "Insert a cmake library"
                         'cmake-tempo-tags)
  )

(provide 'init-cpp)

;;; init-cpp.el ends here
