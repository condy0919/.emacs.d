;;; init-cpp.el --- Cpp -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)

;; Find sibling header/source/test/benchmark files.
(use-package files
  :ensure nil
  :config
  (dolist (rule
           `(;; _test -> headers / sources / benchmark
             (,(rx (group (+ (not "/"))) "_test" (group (or ".cpp" ".cc" ".cxx" ".c")) eos)
              "\\1\\.hpp" "\\1\\.hh" "\\1\\.hxx" "\\1\\.h"
              "\\1\\2" "\\1_benchmark\\2")
             ;; _benchmark -> headers / sources / test
             (,(rx (group (+ (not "/"))) "_benchmark" (group (or ".cpp" ".cc" ".cxx" ".c")) eos)
              "\\1\\.hpp" "\\1\\.hh" "\\1\\.hxx" "\\1\\.h"
              "\\1\\2" "\\1_test\\2")
             ;; headers -> sources / test / benchmark
             (,(rx (group (+ (not "/"))) (or ".hpp" ".hh" ".hxx" ".h") eos)
              "\\1\\.cpp" "\\1\\.cc" "\\1\\.cxx" "\\1\\.c"
              "\\1_test\\.cpp" "\\1_test\\.c" "\\1_benchmark\\.cpp")
             ;; sources -> headers / test / benchmark
             (,(rx (group (+ (not "/"))) (group (or ".cpp" ".cc" ".cxx" ".c")) eos)
              "\\1\\.hpp" "\\1\\.hh" "\\1\\.hxx" "\\1\\.h"
              "\\1_test\\2" "\\1_benchmark\\2")))
    (add-to-list 'find-sibling-rules rule t)))

(use-package cc-mode
  :ensure nil
  :mode ("\\.cxx\\'" . c++-mode)
  :hook (c-mode . (lambda ()
                    (setq comment-start "// "
                          comment-end "")))
  :config
  (with-no-warnings
    (defconst ccls-args nil)
    (defconst clangd-args '("-j=2"
                            "--background-index"
                            "--clang-tidy"
                            "--completion-style=bundled"
                            "--header-insertion-decorators"))
    (with-eval-after-load 'lsp-mode
      ;; Prefer `clangd' over `ccls'
      (cond ((executable-find "clangd") (setq lsp-clients-clangd-executable "clangd"
                                              lsp-clients-clangd-args clangd-args))
            ((executable-find "ccls") (setq lsp-clients-clangd-executable "ccls"
                                            lsp-clients-clangd-args ccls-args)))))
  :custom
  (c-comment-prefix-regexp '((c-mode   . "//+!?\\|\\**")
                             (c++-mode . "//+!?\\|\\**")
                             (awk-mode . "#+")
                             (other    . "//+\\|\\**")))
  (c-doc-comment-style '((c-mode   . gtkdoc)
                         (c++-mode . doxygen)))
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
  (rmsbolt-default-directory temporary-file-directory))

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

;; Snippets for C/C++
(use-package tempo
  :ensure nil
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
                           "#define " (s clause) > n
                           r n
                           "#endif // " (s clause) > n
                           )
                         "ifndef"
                         "Insert #ifndef #define #endif directives"
                         'c-tempo-tags)
  (tempo-define-template "c-extern-C"
                         '("#ifdef __cplusplus" > n
                           "extern \"C\" {" > n
                           "#endif" > n
                           r n
                           "#ifdef __cplusplus" > n
                           "}" > n
                           "#endif" > n
                           )
                         "externC"
                         "Insert #ifdef __cplusplus ..."
                         'c-tempo-tags)
  (tempo-define-template "c-main"
                         '("int main(int argc, char* argv[]) {" > n>
                           p n
                           "}" > n>
                           )
                         "main"
                         "Insert a main function"
                         'c-tempo-tags)

  (tempo-define-template "c++-default-move-op"
                         '((P "class: " class noinsert)
                           (s class) "(" (s class) "&& rhs) noexcept = default;" > n>
                           (s class) "& operator=(" (s class) "&& rhs) noexcept = default;" > n>
                           p n)
                         "mv"
                         "Supply default move ctor and move assignment operator"
                         'c++-tempo-tags)
  (tempo-define-template "c++-default-copy-op"
                         '((P "class: " class noinsert)
                           (s class) "(const " (s class) "& rhs) = default;" > n>
                           (s class) "& operator=(const " (s class) "& rhs) = default;" > n>
                           p n)
                         "cp"
                         "Supply default copy ctor and copy assignment operator"
                         'c++-tempo-tags)
  (tempo-define-template "c++-namespace"
                         '((P "namespace: " ns noinsert)
                           "namespace" (let ((ns (or (tempo-lookup-named 'ns) "")))
                                         (if (string-empty-p ns) "" (concat " " ns)))
                           " {" > n>
                           r n
                           "} // namespace" (let ((ns (or (tempo-lookup-named 'ns) "")))
                                               (if (string-empty-p ns) "" (concat " " ns)))
                           > n)
                         "ns"
                         "Insert a namespace block"
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
  (set-company-backends-for! cmake-mode (company-cmake :with company-tempo) company-dabbrev-code company-dabbrev))

;; Extra font locks for cmake
(use-package cmake-font-lock
  :ensure t
  :hook (cmake-mode . cmake-font-lock-activate))

(defun init-cpp--cmake-upper-id (name)
  "Normalize NAME into an uppercase CMake identifier prefix."
  (replace-regexp-in-string "[^[:alnum:]_]" "_" (upcase (or name ""))))

;; Snippets for cmake
;;
;; Visit https://cmake.org/cmake/help/latest/manual/cmake-buildsystem.7.html for
;; more information about cmake.
(use-package tempo
  :ensure nil
  :hook (cmake-mode . cmake-mode-tempo-setup)
  :config
  (defvar cmake-tempo-tags nil)

  (defun cmake-mode-tempo-setup ()
    (tempo-use-tag-list 'cmake-tempo-tags))

  ;; Recommended project layout (CMake >= 3.21):
  ;;
  ;; .
  ;; ├── CMakeLists.txt           (tag: lib)
  ;; ├── CMakePresets.json        (tag: presets, in js-json-mode/json-ts-mode)
  ;; ├── foo/
  ;; │   ├── CMakeLists.txt       (tag: umbrella)
  ;; │   ├── core/
  ;; │   │   ├── CMakeLists.txt   (tag: comp, test, bench)
  ;; │   │   ├── a.hpp
  ;; │   │   ├── a.cpp
  ;; │   │   ├── a_test.cpp
  ;; │   │   └── a_benchmark.cpp
  ;; │   └── runtime/
  ;; │       └── CMakeLists.txt
  ;; ├── tools/
  ;; │   └── CMakeLists.txt       (tag: exe)
  ;; ├── tests/
  ;; │   └── CMakeLists.txt       (tag: test)
  ;; └── benchmarks/
  ;;     └── CMakeLists.txt       (tag: bench)
  ;;
  ;; Features of `cmake-library' (`lib`):
  ;; - Top-level isolation via `PROJECT_IS_TOP_LEVEL' (safe for FetchContent/add_subdirectory)
  ;; - Decoupled `<PROJ>_BUILD_TESTS' with `enable_testing()' (no global `CTest'/`BUILD_TESTING')
  ;; - Separated `public' and `dev' interface option targets
  (tempo-define-template "cmake-library"
                         '((P "project: " proj noinsert)
                           (tempo-save-named 'PROJ
                                             (init-cpp--cmake-upper-id (tempo-lookup-named 'proj)))
                           "cmake_minimum_required(VERSION 3.21)" n n
                           "project(" (s proj) n
                           "  VERSION 0.1.0" n
                           "  LANGUAGES CXX" n
                           "  DESCRIPTION \"\"" n
                           "  HOMEPAGE_URL \"\"" n
                           ")" n n
                           "# Top-level only settings (do not pollute parent project)" n
                           "if(PROJECT_IS_TOP_LEVEL)" n
                           "  set(CMAKE_POSITION_INDEPENDENT_CODE ON)" n
                           "  set(CMAKE_EXPORT_COMPILE_COMMANDS ON)" n
                           "  if(NOT DEFINED CMAKE_CXX_EXTENSIONS)" n
                           "    set(CMAKE_CXX_EXTENSIONS OFF)" n
                           "  endif()" n
                           "endif()" n n
                           "option(" (s PROJ) "_BUILD_SHARED     \"Build shared libraries\"           OFF)" n
                           "option(" (s PROJ) "_BUILD_TESTS      \"Build unit and integration tests\" ${PROJECT_IS_TOP_LEVEL})" n
                           "option(" (s PROJ) "_BUILD_BENCHMARKS \"Build micro and macro benchmarks\" ${PROJECT_IS_TOP_LEVEL})" n
                           "option(" (s PROJ) "_BUILD_TOOLS      \"Build CLI tools\"                  ${PROJECT_IS_TOP_LEVEL})" n n
                           "### Compile options" n
                           "add_library(" (s proj) "_public_options INTERFACE)" n
                           "target_compile_features(" (s proj) "_public_options INTERFACE cxx_std_23)" n
                           "target_include_directories(" (s proj) "_public_options INTERFACE" n
                           "  ${CMAKE_CURRENT_SOURCE_DIR}" n
                           ")" n n
                           "add_library(" (s proj) "_dev_options INTERFACE)" n
                           "target_compile_options(" (s proj) "_dev_options INTERFACE" n
                           "  $<$<NOT:$<CXX_COMPILER_ID:MSVC>>:-Wall -Wextra -Wpedantic>" n
                           "  $<$<CXX_COMPILER_ID:MSVC>:/W4>" n
                           ")" n n
                           "### Dependencies" n
                           "include(FetchContent)" n n
                           "if(" (s PROJ) "_BUILD_TESTS)" n
                           "  enable_testing()" n
                           "  include(GoogleTest)" n n
                           "  # Targets: GTest::gtest, GTest::gtest_main, GTest::gmock, GTest::gmock_main" n
                           "  FetchContent_Declare(" n
                           "    gtest" n
                           "    GIT_REPOSITORY https://github.com/google/googletest.git" n
                           "    GIT_TAG        v1.16.0" n
                           "    GIT_SHALLOW    TRUE" n
                           "  )" n
                           "  FetchContent_MakeAvailable(gtest)" n
                           "endif()" n n
                           "if(" (s PROJ) "_BUILD_BENCHMARKS)" n
                           "  set(BENCHMARK_ENABLE_TESTING OFF CACHE BOOL \"\" FORCE)" n
                           "  # Targets: benchmark::benchmark, benchmark::benchmark_main" n
                           "  FetchContent_Declare(" n
                           "    benchmark" n
                           "    GIT_REPOSITORY https://github.com/google/benchmark.git" n
                           "    GIT_TAG        v1.9.2" n
                           "    GIT_SHALLOW    TRUE" n
                           "  )" n
                           "  FetchContent_MakeAvailable(benchmark)" n
                           "endif()" n n
                           "### Subdirectories" n
                           "add_subdirectory(" (s proj) ")" n n
                           "if(" (s PROJ) "_BUILD_TOOLS)" n
                           "  add_subdirectory(tools)" n
                           "endif()" n
                           "if(" (s PROJ) "_BUILD_TESTS)" n
                           "  add_subdirectory(tests)" n
                           "endif()" n
                           "if(" (s PROJ) "_BUILD_BENCHMARKS)" n
                           "  add_subdirectory(benchmarks)" n
                           "endif()" n)
                         "lib"
                         "Insert a top-level CMake library project (>= 3.21)"
                         'cmake-tempo-tags)

  (tempo-define-template "cmake-umbrella"
                         '((P "project: " proj noinsert)
                           (tempo-save-named 'PROJ
                                             (init-cpp--cmake-upper-id (tempo-lookup-named 'proj)))
                           "if(" (s PROJ) "_BUILD_SHARED)" n
                           "  set(BUILD_SHARED_LIBS ON)" n
                           "else()" n
                           "  set(BUILD_SHARED_LIBS OFF)" n
                           "endif()" n n
                           p n n
                           "add_library(" (s proj) " INTERFACE)" n
                           "add_library(" (s proj) "::" (s proj) " ALIAS " (s proj) ")" n
                           "target_link_libraries(" (s proj) n
                           "  INTERFACE" n
                           "    " (s proj) "_public_options" n
                           ")" n)
                         "umbrella"
                         "Insert an umbrella target aggregating components"
                         'cmake-tempo-tags)

  (tempo-define-template "cmake-component"
                         '((P "project: " proj noinsert)
                           (P "component: " comp noinsert)
                           "add_library(" (s proj) "_" (s comp) n
                           "  " p n
                           ")" n
                           "add_library(" (s proj) "::" (s comp) " ALIAS " (s proj) "_" (s comp) ")" n n
                           "target_link_libraries(" (s proj) "_" (s comp) n
                           "  PUBLIC" n
                           "    " (s proj) "_public_options" n
                           "  PRIVATE" n
                           "    " (s proj) "_dev_options" n
                           ")" n)
                         "comp"
                         "Insert a CMake component library target"
                         'cmake-tempo-tags)

  (tempo-define-template "cmake-test"
                         '((P "project: " proj noinsert)
                           (P "test target: " target noinsert)
                           (tempo-save-named 'PROJ
                                             (init-cpp--cmake-upper-id (tempo-lookup-named 'proj)))
                           "if(" (s PROJ) "_BUILD_TESTS)" n
                           "  add_executable(" (s target) n
                           "    " (s target) ".cpp" n
                           "  )" n
                           "  target_link_libraries(" (s target) n
                           "    PRIVATE" n
                           "      " (s proj) "::" (p "component or project: ") n
                           "      GTest::gtest_main" n
                           "  )" n
                           "  gtest_discover_tests(" (s target) ")" n
                           "endif()" n)
                         "test"
                         "Insert a GoogleTest executable and CTest registration"
                         'cmake-tempo-tags)

  (tempo-define-template "cmake-benchmark"
                         '((P "project: " proj noinsert)
                           (P "benchmark target: " target noinsert)
                           (tempo-save-named 'PROJ
                                             (init-cpp--cmake-upper-id (tempo-lookup-named 'proj)))
                           "if(" (s PROJ) "_BUILD_BENCHMARKS)" n
                           "  add_executable(" (s target) n
                           "    " (s target) ".cpp" n
                           "  )" n
                           "  target_link_libraries(" (s target) n
                           "    PRIVATE" n
                           "      " (s proj) "::" (p "component or project: ") n
                           "      benchmark::benchmark_main" n
                           "  )" n
                           "endif()" n)
                         "bench"
                         "Insert a Google Benchmark executable"
                         'cmake-tempo-tags)

  (tempo-define-template "cmake-executable"
                         '((P "target: " target noinsert)
                           "add_executable(" (s target) n
                           "  " (s target) ".cpp" n
                           ")" n
                           "target_link_libraries(" (s target) n
                           "  PRIVATE" n
                           "    " p n
                           ")" n)
                         "exe"
                         "Insert an executable target"
                         'cmake-tempo-tags)

  (tempo-define-template "cmake-fetch"
                         '((P "package: " pkg noinsert)
                           "FetchContent_Declare(" n
                           "  " (s pkg) n
                           "  GIT_REPOSITORY " (P "git url: " url) n
                           "  GIT_TAG        " (P "git tag: " tag) n
                           "  GIT_SHALLOW    TRUE" n
                           ")" n
                           "FetchContent_MakeAvailable(" (s pkg) ")" n)
                         "fetch"
                         "Insert a FetchContent dependency block"
                         'cmake-tempo-tags))

;; Snippets for CMakePresets.json (v3, CMake >= 3.21)
(use-package tempo
  :ensure nil
  :hook ((js-json-mode . cmake-presets-tempo-setup)
         (json-ts-mode . cmake-presets-tempo-setup))
  :config
  (defvar cmake-presets-tempo-tags nil)

  (defun cmake-presets-tempo-setup ()
    (when (or (null buffer-file-name)
              (string-match-p (rx "CMake" (? "User") "Presets.json" eos)
                              buffer-file-name))
      (tempo-use-tag-list 'cmake-presets-tempo-tags)))

  (tempo-define-template "cmake-presets"
                         '((P "project: " proj noinsert)
                           (tempo-save-named 'PROJ
                                             (init-cpp--cmake-upper-id (tempo-lookup-named 'proj)))
                           "{" n
                           "  \"version\": 3," n
                           "  \"cmakeMinimumRequired\": {" n
                           "    \"major\": 3," n
                           "    \"minor\": 21," n
                           "    \"patch\": 0" n
                           "  }," n
                           "  \"configurePresets\": [" n
                           "    {" n
                           "      \"name\": \"base\"," n
                           "      \"hidden\": true," n
                           "      \"generator\": \"Ninja\"," n
                           "      \"binaryDir\": \"${sourceDir}/build/${presetName}\"," n
                           "      \"cacheVariables\": {" n
                           "        \"CMAKE_EXPORT_COMPILE_COMMANDS\": \"ON\"" n
                           "      }" n
                           "    }," n
                           "    {" n
                           "      \"name\": \"debug\"," n
                           "      \"inherits\": \"base\"," n
                           "      \"cacheVariables\": {" n
                           "        \"CMAKE_BUILD_TYPE\": \"Debug\"," n
                           "        \"" (s PROJ) "_BUILD_TESTS\": \"ON\"" n
                           "      }" n
                           "    }," n
                           "    {" n
                           "      \"name\": \"release\"," n
                           "      \"inherits\": \"base\"," n
                           "      \"cacheVariables\": {" n
                           "        \"CMAKE_BUILD_TYPE\": \"Release\"," n
                           "        \"" (s PROJ) "_BUILD_TESTS\": \"OFF\"" n
                           "      }" n
                           "    }" n
                           "  ]," n
                           "  \"buildPresets\": [" n
                           "    {" n
                           "      \"name\": \"debug\"," n
                           "      \"configurePreset\": \"debug\"" n
                           "    }," n
                           "    {" n
                           "      \"name\": \"release\"," n
                           "      \"configurePreset\": \"release\"" n
                           "    }" n
                           "  ]," n
                           "  \"testPresets\": [" n
                           "    {" n
                           "      \"name\": \"debug\"," n
                           "      \"configurePreset\": \"debug\"," n
                           "      \"output\": {" n
                           "        \"outputOnFailure\": true" n
                           "      }" n
                           "    }" n
                           "  ]" n
                           "}" n)
                         "presets"
                         "Insert a CMakePresets.json v3 template"
                         'cmake-presets-tempo-tags))

(provide 'init-cpp)

;;; init-cpp.el ends here
