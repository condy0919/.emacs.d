;;; init-dired.el --- dired tweaks -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:

;; Use ( to toggle dired-hide-details-mode
(use-package dired
  :ensure nil
  :custom
  (dired-dwim-target t)
  (dired-auto-revert-buffer t)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-listing-switches "-Afhlv")
  :bind (:map dired-mode-map
         ;; consistent with wgrep
         ("C-c C-e" . wdired-change-to-wdired-mode)))

;; Make dired colorful
(use-package diredfl
  :ensure t
  :hook (dired-mode . diredfl-mode))

(provide 'init-dired)

;;; init-dired.el ends here
