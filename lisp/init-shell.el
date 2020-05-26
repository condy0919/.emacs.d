;;; init-shell.el --- All about shell -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; Beautiful term mode & friends
(use-package vterm
  :ensure t
  :when (or (eq system-type 'gnu/linux)
            (eq system-type 'darwin))
  :commands (evil-insert-state vterm)
  :custom
  (vterm-always-compile-module t)
  (vterm-use-vterm-prompt nil)
  (vterm-kill-buffer-on-exit t)
  (vterm-clear-scrollback-when-clearing t)
  :hook (vterm-mode . (lambda ()
                        (setq-local evil-insert-state-cursor 'box)
                        (setq-local global-hl-line-mode nil)
                        (setq-local truncate-lines t)
                        ;; Dont prompt about processes when killing vterm
                        (setq confirm-kill-processes nil)
                        (evil-insert-state)))
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
  )

;; the Emacs shell
(use-package eshell
  :ensure nil
  :functions eshell/alias
  :custom
  (eshell-kill-on-exit t)
  (eshell-hist-ignoredups t)
  :hook (eshell-mode . (lambda ()
                         ;; Define aliases
                         (eshell/alias "ll" "ls -lh --color=auto")
                         (eshell/alias "vi"   "find-file $1")
                         (eshell/alias "vim"  "find-file $1")
                         (eshell/alias "nvim" "find-file $1")))
  )

;; General term mode
(use-package term
  :ensure nil
  :hook (term-mode . my/term-auto-close)
  :config
  (defun my/term-auto-close ()
    "Close term buffer after exit."
    (when (ignore-errors (get-buffer-process (current-buffer)))
      (set-process-sentinel (get-buffer-process (current-buffer))
                            (lambda (process exit-msg)
                              (when (string-match "\\(finished\\|exited\\)" exit-msg)
                                (kill-buffer (process-buffer process))
                                (when (> (count-windows) 1)
                                  (delete-window)))))))
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
