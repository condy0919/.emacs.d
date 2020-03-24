;;; init-mkey.el --- Keybindings for myself -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Most are copied from `evil-collection'

;;; Code:

(require 'avy)
(require 'evil)
(require 'help-mode)

(defgroup mkey nil
  "Keybindings for myself."
  :group 'convenience)

(defun mkey-open-help-link ()
  "Goto link and open it in `help-mode'."
  (interactive)
  (save-excursion
    (avy-jump "\\.el")
    (push-button)))

;;;###autoload
(defun mkey-help-mode-setup ()
  "Setup `evil' bindings for `help-mode'."
  (evil-set-initial-state 'help-mode 'normal)
  (evil-define-key 'normal 'help-mode-map
)
  

(provide 'init-key)
;;; init-mkey.el ends here
