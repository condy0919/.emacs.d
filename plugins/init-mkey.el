;;; init-mkey.el --- Keybindings for myself -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Most are copied from `evil-collection'.

;;; Code:

(require 'evil)

(defgroup mkey nil
  "Keybindings for myself."
  :group 'convenience)

(defcustom mkey-enable-modes '(help
                               profiler-report
                               occur
                               evil-leader)
  "The list of modes which will be evilified."
  :type '(repeat symbol)
  :group 'mkey)

;;;###autoload
(defun mkey-init ()
  "Register the evil bindings for all modes in `mkey-enable-modes'."
  (interactive)
  (dolist (m mkey-enable-modes)
    (with-eval-after-load m
      (funcall (intern (format "mkey-%s-setup" (symbol-name m)))))))

;;;###autoload
(defun mkey-help-setup ()
  "Setup `evil' bindings for `help-mode'."
  (evil-set-initial-state 'help-mode 'normal)
  (evil-define-key 'normal help-mode-map
    ;; motion
    (kbd "<tab>") 'forward-button
    (kbd "<backtab>") 'backward-button

    ;; quit
    "q" 'quit-window))

;;;###autoload
(defun mkey-profiler-report-setup ()
  "Setup `evil' bindings for `profiler-report'."
  (evil-set-initial-state 'profiler-report-mode 'normal)
  (evil-define-key 'normal profiler-report-mode-map
    ;; toggle
    (kbd "<tab>") 'profiler-report-toggle-entry
    "+"           'profiler-report-expand-entry
    "-"           'profiler-report-collapse-entry

    ;; sort
    "o" 'profiler-report-ascending-sort
    "O" 'profiler-report-descending-sort

    ;; open
    (kbd "RET") 'profiler-report-find-entry

    ;; quit
    "q" 'quit-window))

;;;###autoload
(defun mkey-occur-setup ()
  "Setup `evil' bindings for `occur'."
  (evil-set-initial-state 'occur-mode 'normal)
  (evil-define-key 'normal occur-mode-map
    ;; like `wdired-mode'
    (kbd "C-c C-e") 'occur-edit-mode)

  (evil-define-key 'normal occur-edit-mode-map
    ;; like `wdired-mode'
    (kbd "C-c C-c") 'occur-cease-edit)
  )

;;;###autoload
(defun mkey-evil-leader-setup ()
  "Setup `evil-leader' bindings."
  ;; prefix: <Leader> f, file
  (evil-leader/set-key
    "fj" 'dired-jump
    "ff" 'find-file
    "fd" 'delete-file
    "fc" 'copy-file
    "fr" 'rename-file
    "fg" 'counsel-rg)

  ;; prefix: <Leader> b, buffer
  (evil-leader/set-key
    "bb" 'ivy-switch-buffer
    "bk" 'kill-this-buffer
    "bi" 'ibuffer
    "bp" 'previous-buffer
    "bn" 'next-buffer)

  ;; prefix: <Leader> b, bookmark
  (evil-leader/set-key
    "bm" 'bookmark-set
    "bd" 'bookmark-delete
    "bj" 'bookmark-jump
    "bs" 'bookmark-save)

  ;; prefix: <Leader> w, window
  (evil-leader/set-key
    "w" 'evil-window-map)

  ;; prefix: <Leader> p, projectile
  (evil-leader/set-key
    "pp" 'projectile-switch-project
    "pb" 'projectile-switch-to-buffer
    "pf" 'projectile-find-file
    "pg" 'projectile-ripgrep)

  ;; frequently used keys
  (evil-leader/set-key
    "j" 'avy-goto-word-or-subword-1
    "s" 'avy-goto-char-timer
    "l" 'avy-goto-line
    "i" 'counsel-imenu
    "g" 'counsel-rg)
  )

(provide 'init-mkey)
;;; init-mkey.el ends here
