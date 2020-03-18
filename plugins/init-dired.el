;;; init-dired.el --- dired tweaks -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:

;; use ( to toggle dired-hide-details-mode
(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode)
  :custom
  (dired-dwim-target t)
  (dired-listing-switches "-Afhlv"))

(use-package dired-aux
  :ensure nil
  :custom
  (dired-isearch-filenames 'dwim)
  (dired-create-destination-dirs 'ask)
  (dired-vc-rename-file t)
  :bind (:map dired-mode-map
          ("C-c +" . dired-create-empty-file)))

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
