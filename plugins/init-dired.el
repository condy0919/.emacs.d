;;; init-dired.el --- dired tweaks -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:

;; use ( to toggle dired-hide-details-mode
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

(use-package dired-aux
  :ensure nil
  :hook (dired-mode . dired-isearch-filenames-mode)
  :custom
  (dired-isearch-filenames 'dwim)
  (dired-create-destination-dirs 'ask)
  (dired-vc-rename-file t)
  :bind (:map dired-mode-map
         ("C-c +" . dired-create-empty-file)))

(use-package dired-x
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :custom
  (dired-omit-files (rx (or ".git" ".svn"
                            ".ccls-cache" ".clangd"
                            ".elc" ".pyc" ".o" ".swp")))
  ;; dont prompt about killing buffer visiting delete file
  (dired-clean-confirm-killing-deleted-buffers nil))


;; make dired colorful
(use-package diredfl
  :ensure t
  :hook (dired-mode . diredfl-mode))

;; make all dired operations asynchronous
(use-package async
  :ensure t)

(use-package dired-async
  :ensure nil
  :after (dired async)
  :hook (dired-mode . dired-async-mode))

(provide 'init-dired)

;;; init-dired.el ends here
