;;; init-dired.el --- dired tweaks -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:

;; Use ( to toggle dired-hide-details-mode
(use-package dired
  :ensure nil
  :custom
  (dired-dwim-target t)
  (dired-bind-vm nil)
  (dired-bind-man nil)
  (dired-bind-info nil)
  (dired-bind-jump nil)
  (dired-auto-revert-buffer t)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-listing-switches "-Afhlv")
  :bind (:map dired-mode-map
         ("C-c +"     . dired-create-empty-file)
         ;; consistent with ivy
         ("C-c C-e"   . wdired-change-to-wdired-mode)))

(use-package dired-x
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :bind ([remap list-directory] . dired-jump)
  :custom
  (dired-omit-verbose nil)
  (dired-omit-files (rx (or ".git" ".svn"
                            ".DS_Store"
                            ".cache"
                            ".vscode"
                            ".ccls-cache" ".clangd"
                            ".elc" ".pyc" ".o" ".swp" ".class")))
  ;; Dont prompt about killing buffer visiting delete file
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-guess-shell-alist-user `((,(rx "."
                                        (or
                                         ;; Videos
                                         "mp4" "avi" "mkv" "flv" "ogv" "mov"
                                         ;; Music
                                         "wav" "mp3" "flac"
                                         ;; Images
                                         "jpg" "jpeg" "png" "gif" "xpm" "svg" "bmp"
                                         ;; Docs
                                         "pdf" "md" "djvu" "ps" "eps")
                                        string-end)
                                   ,(cond ((eq system-type 'gnu/linux) "xdg-open")
                                          ((eq system-type 'darwin) "open")
                                          ((eq system-type 'windows-nt) "start")
                                          (t "")))))
   )

;; Make dired colorful
(use-package diredfl
  :ensure t
  :hook (dired-mode . diredfl-mode))

(provide 'init-dired)

;;; init-dired.el ends here
