;;; init-shell.el --- All about shell/term -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'rx)
(require 'init-funcs)

(defun term-mode-common-init ()
  "The common initialization for term/shell."
  (setq-local scroll-margin 0)
  (setq-local truncate-lines t)
  (setq-local global-hl-line-mode nil))

;; General term mode
;;
;; If you use bash, directory track is supported natively.
;; See https://www.emacswiki.org/emacs/AnsiTermHints for more information.
(use-package term
  :ensure nil
  :hook ((term-mode . term-mode-common-init)
         (term-mode . term-mode-prompt-regexp-setup)
         (term-exec . term-mode-no-query))
  :bind (:map term-raw-map
         ("C-c C-y" . term-paste)
         ;; Don't capture my keys!
         ("M-:" . nil)
         ("M-x" . nil)
         ("C-h" . nil)
         ("C-u" . nil))
  :config
  (when (eq system-type 'darwin)
    (define-key term-raw-map (kbd "H-v") 'term-paste))

  (defun term-mode-prompt-regexp-setup ()
    "Setup `term-prompt-regexp' for term-mode."
    (setq-local term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

  (defun term-mode-no-query ()
    "No prompt about processes when killing term."
    (when-let ((proc (ignore-errors (get-buffer-process (current-buffer)))))
      (set-process-query-on-exit-flag proc nil)))
  :custom
  (term-input-ignoredups t)
  (term-completion-autolist t))

;; the Emacs shell & friends
(use-package eshell
  :ensure nil
  :hook ((eshell-mode . (lambda ()
                          (term-mode-common-init)
                          ;; Remove cmd args word by word
                          (modify-syntax-entry ?- "w")
                          (modify-syntax-entry ?. "w")
                          ;; Eshell is not fully functional
                          (setenv "PAGER" "cat")))
         (eshell-after-prompt . eshell-prompt-read-only))
  :config
  ;; Prevent accident typing
  (defalias 'eshell/vi 'find-file)
  (defalias 'eshell/vim 'find-file)

  (defun eshell/bat (file)
    "cat FILE with syntax highlight."
    (with-temp-buffer
      (insert-file-contents file)
      (let ((buffer-file-name file))
        (delay-mode-hooks
          (set-auto-mode)
          (font-lock-ensure)))
      (buffer-string)))

  (defun eshell/f (filename &optional dir)
    "Search for files matching FILENAME in either DIR or the
current directory."
    (let ((cmd (concat
                (executable-find "find")
                " " (or dir ".")
                "      -not -path '*/.git*'"
                " -and -not -path 'build'"    ;; the cmake build directory
                " -and"
                " -type f"
                " -and"
                " -iname '*" filename "*'")))
      (eshell-command-result cmd)))

  (defun eshell-prompt ()
    "Prompt for eshell."
    (concat
     (propertize user-login-name 'face 'font-lock-keyword-face)
     "@"
     "Youmu "
     (if (equal (eshell/pwd) "~")
         "~"
       (abbreviate-file-name (eshell/pwd)))
     " "
     (if-let* ((vc (ignore-errors (vc-responsible-backend default-directory)))
               (br (car (vc-git-branches))))
         (concat (propertize "(" 'face 'success)
                 (format "%s" vc)
                 (propertize ")" 'face 'success)
                 (propertize "-" 'face 'font-lock-string-face)
                 (propertize "[" 'face 'success)
                 (propertize br 'face 'font-lock-constant-face)
                 (propertize "]" 'face 'success)
                 " ")
       "")
     "% "))

  (defun eshell-prompt-read-only ()
    "Make eshell's prompt read-only."
    (add-text-properties
     (point-at-bol)
     (point)
     '(rear-nonsticky t
       field output
       read-only t
       inhibit-line-move-field-capture t)))
  :custom
  (eshell-banner-message "")
  ;; Define our own prompt.
  (eshell-highlight-prompt nil)
  (eshell-prompt-regexp "^[^@]+@[^ ]+ [^ ]+ \\(([a-zA-Z]+)-\\[[a-zA-Z]+\\] \\)?% ")
  (eshell-prompt-function 'eshell-prompt)
  ;; The following cmds will run on term.
  (eshell-visual-commands '("top" "htop" "less" "more" "telnet"))
  (eshell-visual-subcommands '(("git" "help" "lg" "log" "diff" "show")))
  (eshell-visual-options '(("git" "--help" "--paginate")))
  (eshell-destroy-buffer-when-process-dies t)
  ;; Completion like bash
  (eshell-cmpl-ignore-case t)
  (eshell-cmpl-cycle-completions nil))

(use-package em-rebind
  :ensure nil
  :commands eshell-delchar-or-maybe-eof)

(use-package esh-mode
  :ensure nil
  :bind (:map eshell-mode-map
         ([remap kill-region] . backward-kill-word)
         ([remap delete-char] . eshell-delchar-or-maybe-eof)))

;; Used as a `sh-mode' REPL.
;;
;; `shell' is recommended to use over `tramp'.
(use-package shell
  :ensure nil
  :hook (shell-mode . term-mode-common-init))

(provide 'init-shell)
;;; init-shell.el ends here
