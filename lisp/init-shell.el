;;; init-shell.el --- All about shell -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'init-core)

;; Beautiful term mode & friends
(use-package vterm
  :ensure t
  :when (and (bound-and-true-p module-file-suffix)
             (executable-find "cmake")
             (executable-find "make")
             (executable-find "libtool"))
  :custom
  (vterm-buffer-name-string "%s")
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

  (define-advice counsel-yank-pop-action (:around (func &rest args))
    (if (eq major-mode 'vterm-mode)
        (let ((inhibit-read-only t)
              (yank-undo-function (lambda (_start _end) (vterm-undo))))
          (cl-letf (((symbol-function 'insert-for-yank)
                     (lambda (s) (vterm-send-string s t))))
            (apply func args)))
      (apply func args)))
  )

;; Dumb shell
(use-package shell
  :ensure nil
  :hook (shell-mode . my/buffer-auto-close))

;; the Emacs shell & friends
(use-package eshell
  :ensure nil
  :functions eshell/alias
  :custom
  (eshell-scroll-to-bottom-on-input 'all)
  (eshell-scroll-to-bottom-on-output 'all)
  (eshell-kill-on-exit t)
  (eshell-kill-processes-on-exit t)
  (eshell-hist-ignoredups t)
  (eshell-error-if-no-glob t)
  (eshell-glob-case-insensitive t)
  :hook (eshell-mode . (lambda ()
                         ;; Define aliases
                         (eshell/alias "q"    "exit")
                         (eshell/alias "ll"   "ls -lh --color=auto $*")
                         (eshell/alias "vi"   "find-file $1")
                         (eshell/alias "vim"  "find-file $1")
                         (eshell/alias "nvim" "find-file $1")))
  :config
  (define-advice eshell-term-sentinel (:after (process exit-msg))
    "Cleanup the buffer of visual commands."
    (when (string-match "\\(finished\\|exited\\)" exit-msg)
      (kill-buffer (process-buffer process))
      (when (> (count-windows) 1)
        (delete-window))))

  )

(use-package em-hist
  :ensure nil
  :bind (:map eshell-hist-mode-map
         ("M-r" . my/eshell-complete-history))
  :config
  (defun my/eshell-complete-history ()
    "Complete eshell commands history using `completing-read'."
    (interactive)
    (let ((hist (ring-elements eshell-history-ring)))
      (insert (completing-read "> " hist nil t))))
  )

;; General term mode
(use-package term
  :ensure nil
  :hook ((term-mode . my/buffer-auto-close)
         (term-mode . (lambda ()
                        (setq-local global-hl-line-mode nil)
                        (setq-local truncate-lines t)
                        (setq-local scroll-margin 0)))))

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
