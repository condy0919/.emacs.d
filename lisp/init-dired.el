;;; init-dired.el --- dired tweaks -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:

;; Use ( to toggle dired-hide-details-mode
(use-package dired
  :ensure nil
  :custom
  (dired-dwim-target t)
  (dired-bind-man nil)
  (dired-bind-info nil)
  (dired-auto-revert-buffer t)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-listing-switches "-Afhlv")
  :bind (:map dired-mode-map
         ("-" . dired-up-directory)
         ("C-c +" . dired-create-empty-file)
         ;; consistent with wgrep
         ("C-c C-e" . wdired-change-to-wdired-mode)))

(use-package dired-x
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :custom
  (dired-omit-verbose nil)
  (dired-omit-files (rx (or ".git" ".svn"
                            ".cache"
                            ".vscode"
                            ".ccls-cache" ".clangd"
                            ".elc" ".pyc" ".o" ".swp")))
  ;; Dont prompt about killing buffer visiting delete file
  (dired-clean-confirm-killing-deleted-buffers nil))

;; Make dired colorful
(use-package diredfl
  :ensure t
  :hook (dired-mode . diredfl-mode))

(provide 'init-dired)

;;; init-dired.el ends here
