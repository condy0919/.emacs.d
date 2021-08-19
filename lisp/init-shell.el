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

  (defun term-mode-no-query ()
    "No prompt about processes when killing term."
    (when-let ((proc (ignore-errors (get-buffer-process (current-buffer)))))
      (set-process-query-on-exit-flag proc nil)))
  :custom
  (term-input-ignoredups t)
  (term-completion-autolist t)
  (term-scroll-to-bottom-on-output 'all)
  (term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

;; the Emacs shell & friends
(use-package eshell
  :ensure nil
  :defines eshell-prompt-regexp
  :hook ((eshell-mode . (lambda ()
                          (term-mode-common-init)
                          ;; Remove cmd args word by word
                          (modify-syntax-entry ?- "w")
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
    (require 'shrink-path)
    (concat
     (propertize user-login-name 'face 'font-lock-keyword-face)
     "@"
     "Youmu "
     (if (equal (eshell/pwd) "~")
         "~"
       (abbreviate-file-name (shrink-path-file (eshell/pwd))))
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
  (eshell-scroll-to-bottom-on-input 'all)
  (eshell-scroll-to-bottom-on-output 'all)
  (eshell-kill-on-exit t)
  (eshell-kill-processes-on-exit t)
  ;; Don't record command in history if starts with whitespace
  (eshell-input-filter 'eshell-input-filter-initial-space)
  (eshell-error-if-no-glob t)
  (eshell-glob-case-insensitive t)
  (eshell-highlight-prompt nil)
  (eshell-prompt-regexp "^[^@]+@[^ ]+ [^ ]+ \\(([a-zA-Z]+)-\\[[a-zA-Z]+\\] \\)?% ")
  (eshell-prompt-function 'eshell-prompt))

(use-package em-term
  :ensure nil
  :custom
  (eshell-visual-commands '("top" "htop" "less" "more" "bat" "telnet"))
  (eshell-visual-subcommands '(("git" "help" "lg" "log" "diff" "show")))
  (eshell-visual-options '(("git" "--help" "--paginate")))
  (eshell-destroy-buffer-when-process-dies t))

(use-package em-cmpl
  :ensure nil
  :custom
  (eshell-cmpl-autolist t)
  (eshell-cmpl-ignore-case t)
  (eshell-cmpl-cycle-completions nil))

(use-package em-rebind
  :ensure nil
  :commands eshell-delchar-or-maybe-eof)

(use-package esh-mode
  :ensure nil
  :bind (:map eshell-mode-map
         ;; C-n and C-p are widely used in most modes. However, C-n takes
         ;; precedence over [remap next-line] which may cause inconvenience if
         ;; 3rd party packages remap next-line only.
         ;;
         ;; e.g. `next-line' is remapped to `corfu-next' in `corfu-mode'. When
         ;; completion pops up in eshell and C-n is typed, Emacs first trys to
         ;; find C-n in `corfu-map' but nothing is found. Finally
         ;; `eshell-next-input' in `eshell-mode-map' will be found which
         ;; confuses people that overriding keymap (corfu-map is in
         ;; `minor-mode-overriding-map-alist') has a low priority than major
         ;; keymap.
         ([remap next-line]     . eshell-next-input)
         ([remap previous-line] . eshell-previous-input)
         ("C-w" . backward-kill-word)
         ("C-d" . eshell-delchar-or-maybe-eof)))

;; Used as a `sh-mode' REPL.
;;
;; `shell' is recommended to use over `tramp'.
(use-package shell
  :ensure nil
  :hook (shell-mode . term-mode-common-init))

(provide 'init-shell)
;;; init-shell.el ends here
