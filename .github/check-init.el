;;; check-init.el --- Strict batch startup check -*- lexical-binding: t; -*-

;;; Commentary:
;; Run from a checkout with:
;; emacs -Q --batch -l .github/check-init.el -f ci-check-init
;; This checks startup Lisp and hooks, not GUI frames or lazy language tools.

;;; Code:

(require 'package)
(require 'use-package)
(require 'warnings)
(require 'server)

(defvar ci--reported-errors nil
  "Error-level warnings recorded during the startup check.")

(defun ci--record-warning (type message &optional level _buffer-name)
  "Record warnings of TYPE and MESSAGE when LEVEL denotes an error.
Do not signal here: callers such as `use-package' might catch that error."
  (when (memq level '(:error :emergency))
    (push (format "%s: %s" type message) ci--reported-errors)))

(defun ci-check-init (&optional directory)
  "Check startup in DIRECTORY, defaulting to the current checkout.
Missing files, uncaught errors and error-level warnings must fail the check.
Ordinary warnings remain visible but are not fatal."
  (let* ((user-emacs-directory (file-name-as-directory
                              (expand-file-name (or directory default-directory))))
         (user-init-file (expand-file-name "init.el" user-emacs-directory))
         (package-user-dir (expand-file-name "elpa/" user-emacs-directory))
         (package-gnupghome-dir (expand-file-name "gnupg" package-user-dir))
         (package-quickstart-file (expand-file-name "package-quickstart.el"
                                                   user-emacs-directory))
         (user-lisp-directory (expand-file-name "user-lisp/" user-emacs-directory))
         (init-file-debug t)
         (debug-on-error t)
         (use-package-expand-minimally t)
         (before-init-time (current-time))
         (after-init-time nil)
         (after-init-hook (copy-sequence after-init-hook))
         (emacs-startup-hook (copy-sequence emacs-startup-hook))
         ;; Do not collide with a running interactive Emacs during local checks.
         (server-name (format "ci-%s" (emacs-pid)))
         (server-socket-dir (expand-file-name "var/server/" user-emacs-directory))
         (ci--reported-errors nil))
    (advice-add 'display-warning :before #'ci--record-warning)
    (unwind-protect
        (progn
          (load (expand-file-name "early-init.el" user-emacs-directory) nil t)
          (package-initialize)
          (when (file-directory-p user-lisp-directory)
            (prepare-user-lisp (not user-lisp-auto-scrape)))
          (load user-init-file nil t)
          (setq after-init-time (current-time))
          (run-hooks 'after-init-hook)
          (run-hooks 'emacs-startup-hook)
          (when ci--reported-errors
            (error "Startup reported errors:\n%s"
                   (mapconcat #'identity (nreverse ci--reported-errors) "\n")))
          (message "CI startup check passed"))
      (advice-remove 'display-warning #'ci--record-warning))))

(provide 'check-init)
;;; check-init.el ends here
