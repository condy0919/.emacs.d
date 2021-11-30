;;; init-cpp.el --- Cpp -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'init-macros)

(use-package cc-mode
  :ensure nil
  :defines lsp-clients-clangd-executable lsp-clients-clangd-args
  :mode ("\\.cxx\\'" . cc-mode)
  :hook (c-mode . (lambda ()
                    (setq comment-start "// "
                          comment-end "")))
  :config
  (defconst ccls-args nil)
  (defconst clangd-args '("-j=2"
                          "--malloc-trim"
                          "--background-index"
                          "--clang-tidy"
                          "--completion-style=bundled"
                          "--pch-storage=memory"
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
                         (c++-mode . ,(if (>= emacs-major-version 28) 'doxygen 'gtkdoc))))
  (c-basic-offset 4)
  (c-label-minimum-indentation 0)
  (c-offsets-alist '(;; a multi-line C style block comment
                     ;;
                     ;; /**
                     ;;  * text
                     ;;  */
                     ;; int foo();
                     (c                     . c-lineup-C-comments)
                     ;; a multi-line string
                     ;;
                     ;; const char* s = "hello,\
                     ;; world";
                     (string                . c-lineup-dont-change)
                     ;; brace of function
                     ;;
                     ;; int add1(int x) {
                     ;;     return ++x;
                     ;; }
                     (defun-open            . 0)
                     (defun-close           . 0)
                     (defun-block-intro     . +)
                     ;; brace of class
                     ;;
                     ;; class Foo {
                     ;; public:                                 // <- access-label
                     ;; };
                     (class-open            . 0)
                     (class-close           . 0)
                     (access-label          . -)
                     ;; brace of class method
                     ;;
                     ;; class Foo {
                     ;;     friend class Bar;                   // <- friend
                     ;;     int getVar() {                      // <- inclass
                     ;;         return 42;
                     ;;     }
                     ;; };
                     (inline-open           . 0)
                     (inline-close          . 0)
                     (inclass               . +)
                     (friend                . 0)
                     ;; `noexcept' specifier indentation
                     (func-decl-cont        . +)
                     ;; brace of list
                     ;;
                     ;; int nums[] =
                     ;; {
                     ;;     0,
                     ;;     1,
                     ;;     {2},
                     ;; };
                     (brace-list-open       . 0)
                     (brace-list-close      . 0)
                     (brace-list-intro      . +)
                     (brace-list-entry      . 0)
                     (brace-entry-open      . 0)
                     ;; brace of namespace
                     ;;
                     ;; namespace ns {
                     ;; const int var = 42;
                     ;; }
                     (namespace-open        . 0)
                     (namespace-close       . 0)
                     (innamespace           . 0)
                     ;; brace of statement block
                     ;;
                     ;; int send_mail() {
                     ;;     std::mutex io_mtx;
                     ;;     {
                     ;;         std::lock_guard<std::mutex> lk(io_mtx);
                     ;;         // ...
                     ;;     }
                     ;; }
                     (block-open            . 0)
                     (block-close           . 0)
                     ;; topmost definition
                     ;;
                     ;; struct
                     ;; foo {};
                     (topmost-intro         . 0)
                     (topmost-intro-cont    . c-lineup-topmost-intro-cont)
                     ;; class member initialization list
                     ;;
                     ;; struct foo {
                     ;;     foo(int a, int b) :
                     ;;         a_(a),
                     ;;         b_(b) {}
                     ;; };
                     (member-init-intro     . +)
                     (member-init-cont      . c-lineup-multi-inher)
                     ;; class inheritance
                     ;;
                     ;; struct Derived : public Base1,
                     ;;                  public Base2 {
                     ;; };
                     (inher-intro           . +)
                     (inher-cont            . c-lineup-multi-inher)
                     ;; A C statement
                     ;;
                     ;; int main(int argc, char* argv[]) {
                     ;;     const int var1 = 42;
                     ;;     const int var2 = (argc > 1) ? 314   // <- a new statement starts
                     ;;                                 : 512;  // <- statement-cont
                     ;;     {
                     ;;         const int var3 = 42;            // <- statement-block-intro
                     ;;     }
                     ;;
                     ;;     switch (argc) {
                     ;;     case 0:                             // <- case-label
                     ;;         break;                          // <- statement-case-intro
                     ;;
                     ;;     case 1:
                     ;;         {                               // <- statement-case-open
                     ;;             const int tmp = 101;
                     ;;         }
                     ;;         break;
                     ;;     }
                     ;;
                     ;;     if (argc == 1)
                     ;;         assert(argc == 1);              // <- substatement
                     ;;
                     ;;     if (argc == 1)
                     ;;     {                                   // <- substatement-open
                     ;;         assert(argc == 1);
                     ;;     }
                     ;;
                     ;;     // comments                         // <- comment-intro
                     ;;     if (argc == 1)
                     ;;     glabel:                             // <- substatement-label
                     ;;         assert(argc == 1);
                     ;;
                     ;; error:                                  // <- label, with zero `c-label-minimum-indentation'
                     ;;     return -1;
                     ;; }
                     (statement             . 0)
                     (statement-cont        . (c-lineup-ternary-bodies +))
                     (statement-block-intro . +)
                     (statement-case-intro  . +)
                     (statement-case-open   . +)
                     (substatement          . +)
                     (substatement-open     . 0)
                     (substatement-label    . 0)
                     (case-label            . 0)
                     (label                 . 0)
                     (do-while-closure      . 0)
                     (else-clause           . 0)
                     (catch-clause          . 0)
                     (comment-intro         . c-lineup-comment)
                     ;; funcall with arglist
                     ;;
                     ;; sum(
                     ;;     1, 2, 3
                     ;; );
                     (arglist-intro         . +)
                     (arglist-cont          . 0)
                     (arglist-cont-nonempty . c-lineup-arglist)
                     (arglist-close         . c-lineup-close-paren)
                     ;; operator>> and operator<< for cin/cout
                     ;;
                     ;; std::cin >> a
                     ;;          >> b;
                     ;; std::cout << a
                     ;;           << b;
                     (stream-op             . c-lineup-streamop)
                     ;; macros
                     ;;
                     ;; #define ALIST(G)                                \
                     ;;     G(1)                                        \
                     ;;     G(2)
                     (cpp-macro             . -1000)
                     (cpp-macro-cont        . +)
                     ;; extern
                     ;;
                     ;; extern "C" {
                     ;; void test();
                     ;; }
                     (extern-lang-open      . 0)
                     (extern-lang-close     . 0)
                     (inextern-lang         . 0)
                     ;; lambda
                     ;;
                     ;; auto f = [](int a, int b) {
                     ;;     return a + b;
                     ;; };
                     (inlambda              . 0)
                     (lambda-intro-cont     . +)
                     ;; GNU extension, a compound statement as expression
                     ;;
                     ;; int x = 1, y = 2;
                     ;; int z = ({
                     ;;     int ret;
                     ;;     if (y > 0)
                     ;;         ret = y;
                     ;;     else
                     ;;         ret = x - y;
                     ;;     ret;
                     ;; });
                     (inexpr-statement      . 0)
                     ;; c++ template args
                     ;;
                     ;; dummy<int,
                     ;;       char,
                     ;;       double>(0, 0, 0);
                     (template-args-cont    . (c-lineup-template-args +)))))

;; A compiler output viewer
(use-package rmsbolt
  :ensure t
  :commands rmsbolt-compile
  :custom
  (rmsbolt-asm-format nil)
  (rmsbolt-default-directory "/tmp"))

;; Parser generator
(use-package bison-mode
  :ensure t
  :mode (("\\.l\\'" . flex-mode)
         ("\\.y\\'" . bison-mode)))

;; LLVM IR
;;
;; Install with
;;
;; ```elisp
;; (quelpa
;;  '(llvm-mode :fetcher url
;;              :url "https://gitee.com/mirrors/LLVM/raw/main/llvm/utils/emacs/llvm-mode.el"))
;; ```
(use-package llvm-mode
  :ensure nil
  :mode ("\\.ll\\'" . llvm-mode))

;; TableGen description
;;
;; Install with
;;
;; ```elisp
;; (quelpa
;;  '(tablegen-mode :fetcher url
;;                  :url "https://gitee.com/mirrors/LLVM/raw/main/llvm/utils/emacs/tablegen-mode.el"))
;; ```
(use-package tablegen-mode
  :ensure nil
  :mode ("\\.td\\'" . tablegen-mode))

;; Highlight "#if 0" as comments
(use-package hideif
  :ensure nil
  :hook ((c-mode c++-mode) . hide-ifdef-mode)
  :config
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

;; Expand C macros
;;
;; Useful when writing quick tests.
(use-package cmacexp
  :ensure nil
  :commands c-macro-expand
  :custom
  (c-macro-prompt-flag t)
  (c-macro-shrink-window-flag t))

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
                         'c++-tempo-tags))

;; cmake, the de factor build system for C++
(use-package cmake-mode
  :ensure t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'"         . cmake-mode))
  :bind (:map cmake-mode-map
         ;; Compatible with lsp-mode keybindings
         ("C-c d" . cmake-help))
  :config
  (set-company-backends-for! cmake-mode company-cmake))

;; Extra font locks for cmake
(use-package cmake-font-lock
  :ensure t
  :hook (cmake-mode . cmake-font-lock-activate))

;; Snippets for cmake
;;
;; Visit https://cmake.org/cmake/help/latest/manual/cmake-buildsystem.7.html for
;; more information about cmake.
(use-package tempo
  :ensure nil
  :after cmake-mode
  :hook (cmake-mode . cmake-mode-tempo-setup)
  :config
  (defvar cmake-tempo-tags nil)

  (defun cmake-mode-tempo-setup ()
    (tempo-use-tag-list 'cmake-tempo-tags))

  ;; cmake provides `PROJECT_NAME` to refer the name defined with `project`
  ;; function. Sadly converting a string to upper case is hard in cmake.
  (defun proj-prefixed (arg)
    (tempo-process-and-insert-string (concat (upcase (tempo-lookup-named 'proj)) arg)))

  ;; Make sure you have the following directory hierarchy
  ;;
  ;; .
  ;; ├── CMakeLists.txt
  ;; ├── include
  ;; │   └── lib.hpp
  ;; ├── src
  ;; │   └── lib.cpp
  ;; ├── benchmarks
  ;; │   └── CMakeLists.txt
  ;; └── tests
  ;;     └── CMakeLists.txt
  ;;
  ;; The `benchmarks` directory is optional.
  ;;
  ;; If you want to put all headers in `src` directory, don't forget to export them.
  ;;
  ;; ``` cmake
  ;; # method 1
  ;; set(CMAKE_INCLUDE_CURRENT_DIR_IN_INTERFACE ON)
  ;;
  ;; # method 2
  ;; target_sources(yourlib
  ;;   PRIVATE
  ;;     implementation.cpp
  ;;   PUBLIC
  ;;     header.hpp
  ;; )
  ;; ```
  ;;
  ;; # Features
  ;;
  ;; - Option to enable tests
  ;; - Option to enabel benchmarks
  ;; - Git tag based version
  (tempo-define-template "cmake-library"
                         '((P "project: " proj 'noinsert)
                           "cmake_minimum_required(VERSION 3.11)" n n
                           "set(CMAKE_POSITION_INDEPENDENT_CODE ON)" n
                           "set(CMAKE_EXPORT_COMPILE_COMMANDS ON)" n n
                           "project(" (s proj) n
                           "  VERSION 0.1.0" n
                           "  LANGUAGES CXX" n
                           "  DESCRIPTION \"\"" n
                           "  HOMEPAGE_URL \"\"" n
                           ")" n n
                           "include(CTest)" n
                           "include(FetchContent)" n n
                           "option(" (proj-prefixed "_ENABLE_TESTS") " \"Enable tests\" ON)" n
                           "option(" (proj-prefixed "_ENABLE_BENCHMARKS") " \"Enable benchmarks\" OFF)" n n
                           "# Sources for the library are specified at the end" n
                           "add_library(" (s proj) " \"\")" n n
                           "# Alias to avoid name conflicts" n
                           "# add_library(ANameWithDoubleColons ALIAS " (s proj) ")" n n
                           "### Commpile options" n
                           "# Enable C++17 (Required)" n
                           "target_compile_features(" (s proj) n
                           "  PUBLIC" n
                           "    cxx_std_17" n
                           ")" n n
                           "# Common GCC/Clang options" n
                           "target_compile_options(" (s proj) n
                           "  PRIVATE" n
                           "    $<$<NOT:$<CXX_COMPILER_ID:MSVC>>:-Wall -Wextra>" n
                           ")" n n
                           "### Libraries" n
                           "# Enable threading support" n
                           "set(THREADS_PREFER_PTHREAD_FLAG ON)" n
                           "find_package(Threads REQUIRED)" n
                           "target_link_libraries(" (s proj) " PRIVATE Threads::Threads)" n n
                           "# Benchmark" n
                           "if(" (proj-prefixed "_ENABLE_BENCHMARKS)") n
                           "  set(BENCHMARK_ENABLE_TESTING OFF CACHE BOOL \"Disable benchmark testing\" FORCE)" n
                           "  FetchContent_Declare(" n
                           "    benchmark" n
                           "    GIT_REPOSITORY https://github.com/google/benchmark.git" n
                           "    GIT_TAG        v1.6.0" n
                           "    GIT_SHALLOW    true" n
                           "    GIT_PROGRESS   true" n
                           "  )" n
                           "  FetchContent_MakeAvailable(benchmark)" n n
                           "  add_subdirectory(benchmarks)" n
                           "endif()" n n
                           "# Test" n
                           "if(" (proj-prefixed "_ENABLE_TESTS)") n
                           "  # libdoctest.a and libdoctest_with_main.a will be built" n
                           "  FetchContent_Declare(" n
                           "    doctest" n
                           "    GIT_REPOSITORY https://github.com/onqtam/doctest" n
                           "    GIT_TAG        2.4.6" n
                           "    GIT_SHALLOW    true" n
                           "    GIT_PROGRESS   true" n
                           "  )" n
                           "  FetchContent_MakeAvailable(doctest)" n n
                           "  add_subdirectory(tests)" n
                           "endif()" n n
                           "### Definitions" n n
                           "### Includes" n n
                           "target_include_directories(" (s proj) " PUBLIC" n
                           "  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>" n
                           "  $<INSTALL_INTERFACE:include>" n
                           ")" n n
                           "### Install" n n
                           "### Sources" n
                           "target_sources(" (s proj) n
                           "  PRIVATE" n
                           "    src/lib.cpp" n
                           ")" n n
                           "### Obtain version information from Git" n
                           "# if(NOT " (proj-prefixed "_VERSION)") n
                           "#   execute_process(COMMAND git describe --tag --long HEAD" n
                           "#     OUTPUT_VARIABLE " (proj-prefixed "_VERSION") n
                           "#     OUTPUT_STRIP_TRAILING_WHITESPACE" n
                           "#     WORKING_DIRECTORY \"${CMAKE_CURRENT_SOURCE_DIR}\")" n
                           "#" n
                           "#   if(NOT " (proj-prefixed "_VERSION)") n
                           "#     set(" (proj-prefixed "_VERSION") " \"<unknown>\")" n
                           "#   endif()" n
                           "# endif()" n n
                           "# target_compile_definitions(" (s proj) " PRIVATE " (proj-prefixed "_VERSION=\"${") (proj-prefixed "_VERSION") "}\")" n
                           )
                         "lib"
                         "Insert a cmake library"
                         'cmake-tempo-tags)
  )

(provide 'init-cpp)

;;; init-cpp.el ends here
