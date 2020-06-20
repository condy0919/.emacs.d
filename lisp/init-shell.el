;;; init-shell.el --- All about shell -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'init-core)

(defun my/term-mode-common-init ()
  "The common initialization for term."
  (setq-local scroll-margin 0)
  (setq-local truncate-lines t)
  (setq-local global-hl-line-mode nil)
  (when (bound-and-true-p evil-mode)
    (setq-local evil-insert-state-cursor 'box)
    (evil-insert-state))
  )

;; Beautiful term mode & friends
(use-package vterm
  :ensure t
  :when (and (bound-and-true-p module-file-suffix)
             (executable-find "cmake")
             (executable-find "make")
             (executable-find "libtool"))
  :custom
  (vterm-always-compile-module t)
  (vterm-use-vterm-prompt nil)
  (vterm-kill-buffer-on-exit t)
  (vterm-clear-scrollback-when-clearing t)
  :hook (vterm-mode . (lambda ()
                        (my/term-mode-common-init)
                        ;; Dont prompt about processes when killing vterm
                        (setq confirm-kill-processes nil)))
  :config
  ;; Directory synchronization (linux-only)
  (defun my/vterm-directory-sync ()
    "Synchronize current working directory."
    (when vterm--process
      (let* ((pid (process-id vterm--process))
             (dir (file-truename (format "/proc/%d/cwd/" pid))))
        (setq default-directory dir))))

  (when (eq system-type 'gnu/linux)
    (define-advice vterm-send-return (:after nil)
      "Synchronize current working directory."
      (run-with-idle-timer 0.1 nil 'my/vterm-directory-sync)))

  (define-advice counsel-yank-pop-action (:around (func &rest args))
    (if (eq major-mode 'vterm-mode)
        (let ((inhibit-read-only t)
              (yank-undo-function (lambda (_start _end) (vterm-undo))))
          (cl-letf (((symbol-function 'insert-for-yank)
                     (lambda (s) (vterm-send-string s t))))
            (apply func args)))
      (apply func args)))
  )

;; the Emacs shell & friends
(use-package eshell
  :ensure nil
  :functions eshell/alias
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
  (eshell-hist-ignoredups t)
  (eshell-input-filter 'eshell-input-filter-initial-space)
  (eshell-glob-case-insensitive t)
  (eshell-highlight-prompt nil)
  (eshell-prompt-regexp "^[^@]+@[^ ]+ [^ ]+ \\(([a-zA-Z]+)-\\[[a-zA-Z]+\\] \\)?% ")
  (eshell-prompt-function 'my/eshell-prompt)
  :hook ((eshell-mode . (lambda ()
                          (my/term-mode-common-init)
                          ;; Define aliases
                          (eshell/alias "ll"   "ls -lh --color=auto $*")
                          (eshell/alias "vi"   "find-file $1")
                          (eshell/alias "vim"  "find-file $1")
                          (eshell/alias "nvim" "find-file $1")))
         (eshell-first-time-mode . (lambda ()
                                     (evil-collection-define-key 'insert 'eshell-mode-map
                                       (kbd "C-w") 'backward-kill-word
                                       (kbd "C-d") 'my/eshell-quit-or-delete-char
                                       (kbd "C-p") 'eshell-previous-input
                                       (kbd "C-n") 'eshell-next-input))))
  :config
  (define-advice eshell-term-sentinel (:after (process exit-msg))
    "Cleanup the buffer of visual commands."
    (when (string-match "\\(finished\\|exited\\)" exit-msg)
      (kill-buffer (process-buffer process))
      (when (> (count-windows) 1)
        (delete-window))))

  ;; Copy from doom
  (defun my/eshell-quit-or-delete-char (arg)
    "Delete a character or quit eshell if there's nothing to delete."
    (interactive "p")
    (if (and (eolp) (looking-back eshell-prompt-regexp nil))
        (eshell-life-is-too-much)
      (delete-char arg)))

  (defun my/eshell-prompt ()
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
     (if-let* ((vc (ignore-errors (vc-responsible-backend default-directory))))
         (concat (propertize "(" 'face 'success)
                 (format "%s" vc)
                 (propertize ")" 'face 'success)
                 (propertize "-" 'face 'font-lock-string-face)
                 (propertize "[" 'face 'success)
                 (propertize (car (vc-git-branches)) 'face 'font-lock-constant-face)
                 (propertize "]" 'face 'success)
                 " ")
       "")
     "% "))
  )

(use-package em-hist
  :ensure nil
  :bind (:map eshell-hist-mode-map
         ("M-r" . my/eshell-complete-history))
  :config
  (defun my/eshell-complete-history ()
    "Complete eshell commands history using `completing-read'."
    (interactive)
    (let ((hist (delete-dups (ring-elements eshell-history-ring))))
      (insert (completing-read "> " hist nil t))))
  )

;; Popup a shell
(use-package shell-pop
  :ensure t
  :bind ("M-=" . shell-pop)
  :custom
  (shell-pop-window-size 40)
  (shell-pop-full-span t)
  (shell-pop-shell-type (if (fboundp 'vterm) '("vterm" "*vterm*" #'vterm)
                          '("eshell" "*eshell*" #'eshell))))

(provide 'init-shell)

;;; init-shell.el ends here
