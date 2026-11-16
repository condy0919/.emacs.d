;;; check-init-tests.el --- Regression tests for the CI loader -*- lexical-binding: t; -*-

;;; Commentary:
;; These tests run isolated Emacs subprocesses without downloading packages.
;; emacs -Q --batch -l .github/check-init-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'cl-lib)
(require 'ert)

(defconst ci-test--loader
  (expand-file-name "check-init.el" (file-name-directory load-file-name)))

(cl-defun ci-test--run (init &optional (early "") extra-files)
  "Run the loader with INIT, EARLY and EXTRA-FILES in a temporary checkout.
Use :missing for an absent startup file.  Return (EXIT-CODE . OUTPUT)."
  (let ((directory (make-temp-file "emacs-ci-test-" t)))
    (unwind-protect
        (progn
          (dolist (entry (append `(("early-init.el" . ,early)
                                  ("init.el" . ,init))
                                extra-files))
            (unless (eq (cdr entry) :missing)
              (let ((file (expand-file-name (car entry) directory)))
                (make-directory (file-name-directory file) t)
                (with-temp-file file
                  (insert ";;; -*- lexical-binding: t; -*-\n" (cdr entry))))))
          (with-temp-buffer
            (let* ((default-directory (file-name-as-directory directory))
                   (status (call-process
                            (expand-file-name invocation-name invocation-directory)
                            nil (list (current-buffer) t) nil
                            "-Q" "--batch" "-l" ci-test--loader
                            "-f" "ci-check-init")))
              (cons status (buffer-string)))))
      (delete-directory directory t))))

(defun ci-test--assert-failure (result &rest diagnostics)
  "Assert RESULT failed and contains every expected DIAGNOSTICS string."
  (should (integerp (car result)))
  (should-not (zerop (car result)))
  ;; Ignore indented backtrace frames: their printed source can contain a
  ;; fixture's error marker even when an unrelated error interrupted it.
  (let ((output (mapconcat #'identity
                          (cl-remove-if
                           (lambda (line) (string-match-p "\\`[ \t]" line))
                           (split-string (cdr result) "\n"))
                          "\n")))
    (dolist (diagnostic diagnostics)
      (should (string-match-p (regexp-quote diagnostic) output))))
  (should-not (string-match-p "^CI startup check passed$" (cdr result))))

(ert-deftest ci-check-startup-phase-order ()
  (let ((result
         (ci-test--run
          "(unless (equal ci-test-order '(early)) (error \"early-init was skipped\"))
(unless (and init-file-debug debug-on-error) (error \"debug flags missing\"))
(unless (null after-init-time) (error \"after-init-time set too soon\"))
(push 'init ci-test-order)
(add-hook 'after-init-hook
          (lambda ()
            (unless (and after-init-time (equal ci-test-order '(init early)))
              (error \"after-init order incorrect\"))
            (push 'after-init ci-test-order)))
(add-hook 'emacs-startup-hook
          (lambda ()
            (unless (equal ci-test-order '(after-init init early))
              (error \"startup order incorrect\"))
            (message \"CI phase order verified\")))"
          "(defvar ci-test-order '(early))")))
    (should (equal (car result) 0))
    (should (string-match-p "CI phase order verified" (cdr result)))
    (should (string-match-p "^CI startup check passed$" (cdr result)))))

(ert-deftest ci-check-missing-early-init ()
  (ci-test--assert-failure (ci-test--run "" :missing)
                           "Cannot open load file" "early-init.el"))

(ert-deftest ci-check-missing-init ()
  (ci-test--assert-failure (ci-test--run :missing)
                           "Cannot open load file" "/init.el"))

(ert-deftest ci-check-top-level-error ()
  (ci-test--assert-failure (ci-test--run "(error \"CI top-level failure\")")
                           "CI top-level failure"))

(ert-deftest ci-check-early-init-error ()
  (ci-test--assert-failure (ci-test--run "" "(error \"CI early-init failure\")")
                           "CI early-init failure"))

(ert-deftest ci-check-use-package-init-error ()
  (ci-test--assert-failure
   (ci-test--run "(use-package emacs :init (error \"CI init failure\"))")
   "CI init failure"))

(ert-deftest ci-check-use-package-config-error ()
  (ci-test--assert-failure
   (ci-test--run "(use-package emacs :config (error \"CI config failure\"))")
   "CI config failure"))

(ert-deftest ci-check-missing-required-package ()
  (ci-test--assert-failure
   (ci-test--run "(use-package ci-test-nonexistent-package :ensure nil :demand t)")
   "Cannot open load file" "ci-test-nonexistent-package"))

(ert-deftest ci-check-malformed-use-package ()
  (ci-test--assert-failure
   (ci-test--run "(use-package emacs :ci-invalid-keyword t)")
   "Unrecognized keyword: :ci-invalid-keyword"))

(ert-deftest ci-check-swallowed-install-error ()
  (ci-test--assert-failure
   (ci-test--run
    "(require 'cl-lib)
(setq debug-on-error nil)
(cl-letf (((symbol-function 'package-refresh-contents)
           (lambda (&rest _) (error \"CI simulated download failure\"))))
  (use-package ci-test-nonexistent-package :ensure t :defer t))")
   "CI simulated download failure" "Startup reported errors"))

(ert-deftest ci-check-explicit-use-package-catch ()
  (ci-test--assert-failure
   (ci-test--run
    "(setq debug-on-error nil)
(use-package emacs :catch t :config (error \"CI caught config failure\"))")
   "CI caught config failure" "Startup reported errors"))

(ert-deftest ci-check-error-level-warnings ()
  (dolist (level '(:error :emergency))
    (ci-test--assert-failure
     (ci-test--run (format "(display-warning 'ci-test \"CI explicit warning\" %S)" level))
     "CI explicit warning" "Startup reported errors")))

(ert-deftest ci-check-ordinary-warning-is-allowed ()
  (let ((result (ci-test--run "(display-warning 'ci-test \"CI ordinary warning\" :warning)")))
    (should (equal (car result) 0))
    (should (string-match-p "CI ordinary warning" (cdr result)))
    (should (string-match-p "^CI startup check passed$" (cdr result)))))

(ert-deftest ci-check-startup-hook-errors ()
  (dolist (hook '(after-init-hook emacs-startup-hook))
    (ci-test--assert-failure
     (ci-test--run (format "(add-hook '%S (lambda () (error \"CI %S failed\")))" hook hook))
     (format "CI %S failed" hook))))

(ert-deftest ci-check-user-lisp-autoloads ()
  (let ((result
         (ci-test--run
          "(unless (autoloadp (symbol-function 'ci-test-user-command))
  (error \"CI user-lisp autoload missing\"))
(unless (eq (ci-test-user-command) 'ci-user-lisp-ok)
  (error \"CI user-lisp command failed\"))
(message \"CI user-lisp autoload verified\")"
          ;; This fixture tests scraping and byte compilation; don't launch
          ;; asynchronous native compiler processes that outlive its directory.
          "(advice-add 'native-comp-available-p :override (lambda () nil))"
          '(("user-lisp/ci-test-user.el" .
             ";;;###autoload
(defun ci-test-user-command ()
  \"Return the fixture's marker.\"
  'ci-user-lisp-ok)
(provide 'ci-test-user)")))))
    (should (equal (car result) 0))
    (should (string-match-p "CI user-lisp autoload verified" (cdr result)))
    (should (string-match-p "^CI startup check passed$" (cdr result)))))

;;; check-init-tests.el ends here
