;;; init-dired.el --- dired tweaks -*- lexical-binding: t -*-
;;; Commentary:
;;
;; dired-narrow is superseded by `consult-focus-lines'.

;;; Code:

;; Use ( to toggle dired-hide-details-mode
(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
         ;; consistent with ivy
         ("C-c C-e"   . wdired-change-to-wdired-mode))
  :custom
  (dired-dwim-target t)
  (dired-mouse-drag-files t)
  (dired-kill-when-opening-new-dired-buffer t)
  ;; Dont prompt about killing buffer visiting delete file
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-auto-revert-buffer #'dired-directory-changed-p)
  (dired-hide-details-hide-symlink-targets nil)
  ;; Preserve size (5) and timestamp (6 7 8) columns under "-AFhlv"
  (dired-hide-details-preserved-columns '(5 6 7 8))
  (dired-listing-switches "-AFhlv"))

(use-package dired-aux
  :ensure nil
  :defer t
  :custom
  (dired-vc-rename-file t)
  (dired-do-revert-buffer t)
  (dired-isearch-filenames 'dwim)
  (dired-create-destination-dirs 'ask))

;; Use C-x M-o to toggle dired-omit-mode
(use-package dired-x
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :custom
  (dired-omit-verbose nil)
  (dired-omit-files (rx string-start (or "." "#"))))

;; Make dired colorful
(use-package diredfl
  :ensure t
  :hook (dired-mode . diredfl-mode))

(provide 'init-dired)
;;; init-dired.el ends here
