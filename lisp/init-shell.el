;;; init-shell.el --- All about shell/term -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defun shell-mode-common-init ()
  "The common initialization procedure for term/shell."
  (setq-local scroll-margin 0)
  (setq-local truncate-lines t)
  (setq-local global-hl-line-mode nil)
  ;; Force "ls foo" to trigger a completion when foo1, foo2, ... in current directory.
  ;; Replace only the `file' entry so other global category overrides remain intact.
  (setq-local completion-category-overrides
              (cons '(file (styles basic))
                    (assq-delete-all 'file (copy-sequence completion-category-overrides)))))

;; General term mode
;;
;; If you use bash, directory track is supported natively.
;; See https://www.emacswiki.org/emacs/AnsiTermHints for more information.
(use-package term
  :ensure nil
  :hook ((term-mode . shell-mode-common-init)
         (term-mode . term-mode-prompt-regexp-setup))
  :config
  (defun term-mode-prompt-regexp-setup ()
    "Setup `term-prompt-regexp' for term-mode."
    (setq-local term-prompt-regexp "^[^#$%>\n]*[#$%>] *")))

;; The Emacs shell & friends
(use-package eshell
  :ensure nil
  :hook (eshell-mode . shell-mode-common-init)
  :config
  ;; Prevent accident typing
  (defalias 'eshell/vi 'find-file)
  (defalias 'eshell/vim 'find-file)
  (defalias 'eshell/nvim 'find-file)

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
    (find-dired (or dir ".")
                (concat " -not -path '*/.git*'"
                        " -and -not -path 'build'" ;; the cmake build directory
                        " -and"
                        " -type f"
                        " -and"
                        " -iname " (format "'*%s*'" filename))))

  (defun eshell/z ()
    "cd to directory with completions."
    (let ((dir (completing-read "Directory: " (delete-dups (ring-elements eshell-last-dir-ring)) nil t)))
      (eshell/cd dir)))

  (defun eshell/rg (&rest args)
    "ripgrep with eshell integration."
    (eshell-grep "rg" (append '("--no-heading") args) t))
  :custom
  ;; The following cmds will run on term.
  (eshell-visual-commands '("top" "htop" "less" "more" "telnet"))
  (eshell-visual-subcommands '(("git" "help" "lg" "log" "diff" "show")))
  (eshell-visual-options '(("git" "--help" "--paginate")))
  (eshell-cmpl-ignore-case t)
  (eshell-last-dir-ring-size 256))

(use-package em-hist
  :ensure nil
  :bind (:map eshell-hist-mode-map
         ("M-r" . consult-history))
  :custom
  (eshell-history-append t)
  (eshell-hist-ignoredups t)
  (eshell-history-size 10000)
  (eshell-history-isearch 'dwim)
  (eshell-input-filter #'eshell-input-filter-initial-space))

(use-package em-rebind
  :ensure nil
  :commands eshell-delchar-or-maybe-eof)

(use-package esh-mode
  :ensure nil
  :bind (:map eshell-mode-map
         ([remap delete-char] . eshell-delchar-or-maybe-eof))
  :custom
  (eshell-scroll-to-bottom-on-input 'this))

;; The interactive shell.
;;
;; It can be used as a `sh-mode' REPL.
;;
;; `shell' is recommended to use over `tramp'.
(use-package shell
  :ensure nil
  :hook ((shell-mode . shell-mode-common-init)
         (shell-mode . revert-tab-width-to-default))
  :config
  ;; Correct indentation for `ls'
  (defun revert-tab-width-to-default ()
    "Revert `tab-width' to default value."
    (setq-local tab-width 8))
  :custom
  (shell-kill-buffer-on-exit t)
  (shell-get-old-input-include-continuation-lines t))

;; Use eshell everywhere.
;;
;; `eshell' is recommended to use over `tramp'.
(use-package eshell
  :ensure nil
  :bind ("M-`" . eshell-toggle)
  :config
  (defun eshell-toggle ()
    "Toggle a persistent eshell popup window.
If popup is visible but unselected, select it.
If popup is focused, hide it."
    (interactive)
    (if-let* ((win (get-buffer-window "*eshell-popup*")))
        (if (eq (selected-window) win)
            (quit-window nil win)
          (select-window win))
      (let ((display-buffer-alist
             (cons '("\\`\\*eshell-popup\\*\\'"
                     (display-buffer-at-bottom)
                     (dedicated . soft))
                   display-buffer-alist))
            (eshell-buffer-name "*eshell-popup*"))
        (eshell)))))

(provide 'init-shell)
;;; init-shell.el ends here
