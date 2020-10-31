;;; init-shell.el --- All about shell -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(eval-when-compile
  (require 'rx)
  (require 'init-core))

(defun term-mode-common-init ()
  "The common initialization for term."
  (setq-local scroll-margin 0)
  (setq-local truncate-lines t)
  (setq-local global-hl-line-mode nil)
  (when (bound-and-true-p evil-mode)
    (setq-local evil-insert-state-cursor 'box)
    (evil-insert-state)))

;; General term mode
(use-package term
  :ensure nil
  :hook (term-mode . (lambda ()
                       (term-mode-common-init)
                       (my/buffer-auto-close)
                       (when-let* ((proc (ignore-errors (get-buffer-process (current-buffer)))))
                         ;; Don't prompt about processes when killing term
                         (set-process-query-on-exit-flag proc nil)
                         (setq-local term--process proc))))
  :bind (:map term-raw-map
         ("C-c C-y" . term-paste)
         ;; Don't capture my keys!
         ("M-o" . nil)
         ("M-=" . nil)
         ("M-:" . nil)
         ("M-x" . nil)
         ("C-h" . nil)
         ("C-x" . nil)
         ("C-u" . nil))
  :config
  (when (eq system-type 'darwin)
    (define-key term-raw-map (kbd "H-v") 'term-paste))

  (defvar term--process nil)

  ;; Directory synchronization (linux-only)
  (defun term-directory-sync ()
    "Synchronize current working directories."
    (when term--process
      (when-let* ((pid (process-id term--process))
                  (dir (file-truename (format "/proc/%d/cwd/" pid))))
        (setq default-directory dir))))
  :custom
  (term-input-ignoredups t)
  (term-completion-autolist t)
  (term-scroll-to-bottom-on-output 'all))

;; the Emacs shell & friends
(use-package eshell
  :ensure nil
  :defines eshell-prompt-regexp
  :functions eshell/alias
  :hook (eshell-mode . (lambda ()
                         (term-mode-common-init)
                         ;; Remove cmd args word by word
                         (modify-syntax-entry ?- "w")
                         ;; Eshell is not fully functional
                         (setenv "PAGER" "cat")
                         ;; Define aliases
                         (eshell/alias "vi"  "find-file $1")
                         (eshell/alias "vim" "find-file $1")))
  :config
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
  :custom
  (eshell-banner-message
   '(format "%s %s\n"
            (propertize (format " %s " (string-trim (buffer-name)))
                        'face 'mode-line-highlight)
            (propertize (current-time-string)
                        'face 'font-lock-keyword-face)))
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

(use-package em-hist
  :ensure nil
  :bind (:map eshell-hist-mode-map
         ("M-r" . eshell-complete-history))
  :config
  (defun eshell-complete-history ()
    "Complete eshell commands history using `completing-read'."
    (interactive)
    (let ((hist (delete-dups (ring-elements eshell-history-ring))))
      (insert (completing-read "> " hist nil t))))
  :custom
  (eshell-history-size 1024)
  (eshell-hist-ignoredups t)
  (eshell-save-history-on-exit t))

(use-package em-term
  :ensure nil
  :custom
  (eshell-visual-commands '("top" "htop" "less" "more" "bat"))
  (eshell-visual-subcommands '(("git" "help" "lg" "log" "diff" "show")))
  (eshell-visual-options '(("git" "--help" "--paginate")))
  (eshell-destroy-buffer-when-process-dies t))

(use-package em-cmpl
  :ensure nil
  :custom
  (eshell-cmpl-autolist t)
  (eshell-cmpl-ignore-case t)
  (eshell-cmpl-cycle-completions nil)
  (eshell-cmpl-dir-ignore (rx line-start
                              (or "." ".." "CVS" ".svn" ".git")
                              line-end))
  (eshell-cmpl-file-ignore (rx (or "~" ".elc" ".pyc" ".swp")
                               line-end)))

(use-package esh-mode
  :ensure nil
  :bind (:map eshell-mode-map
         ("C-w" . backward-kill-word)
         ("C-d" . eshell-quit-or-delete-char)
         ("C-p" . eshell-previous-input)
         ("C-n" . eshell-next-input))
  :config
  (defun eshell-quit-or-delete-char (arg)
    "Delete a character or quit eshell if there's nothing to delete."
    (interactive "p")
    (if (and (eolp) (looking-back eshell-prompt-regexp nil))
        (eshell-life-is-too-much)
      (delete-char arg)))
  :custom
  ;; !3 to run the third history command
  (eshell-expand-input-functions '(eshell-expand-history-references)))

;; Popup a shell inside Emacs
(use-package shell-pop
  :ensure t
  :bind ("M-=" . shell-pop)
  :custom
  (shell-pop-full-span t)
  (shell-pop-window-size 40)
  (shell-pop-shell-type (if (not (eq system-type 'windows-nt))
                            '("terminal" "*terminal*" (lambda () (term shell-pop-term-shell)))
                          '("eshell" "*eshell*" #'eshell))))

(provide 'init-shell)
;;; init-shell.el ends here
