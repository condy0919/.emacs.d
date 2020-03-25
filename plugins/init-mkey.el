;;; init-mkey.el --- Keybindings for myself -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Most are copied from `evil-collection'.

;;; Code:

(require 'evil)

(defgroup mkey nil
  "Keybindings for myself."
  :group 'convenience)

(defcustom mkey-enable-modes '(help profiler-report occur)
  "The list of modes which will be evilified."
  :type '(repeat symbol)
  :group 'mkey)

;;;###autoload
(defun mkey-init (&rest _)
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

(provide 'init-mkey)
;;; init-mkey.el ends here
