;;; init-key.el --- Keybindings for myself -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'avy)

(defgroup mkey nil
  "Keybindings for myself."
  :group 'convenience)

(defun mkey-open-help-link ()
  "Goto link and open it in `help-mode'."
  (interactive)
  (save-excursion
    (avy-jump ".el")
    (push-button)))

;; help-mode
(with-eval-after-load 'help
  (bind-key "o" #'mkey-open-help-link help-mode-map))


(provide 'init-key)
;;; init-key.el ends here
