;;; init-dired.el --- dired tweaks -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:

(eval-when-compile
  (require 'rx))

;; Use ( to toggle dired-hide-details-mode
(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
         ;; consistent with ivy
         ("C-c C-e"   . wdired-change-to-wdired-mode))
  :custom
  (dired-dwim-target t)
  (dired-bind-vm nil)
  (dired-bind-man nil)
  (dired-bind-info nil)
  (dired-auto-revert-buffer t)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-listing-switches "-Afhlv"))

(use-package dired-aux
  :ensure nil
  :after dired
  :bind (:map dired-mode-map
         ("C-c +" . dired-create-empty-file))
  :config
  ;; with the help of `evil-collection', P is bound to `dired-do-print'.
  (define-advice dired-do-print (:override (&optional _))
    "Show/hide dotfiles."
    (interactive)
    (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p)
        (progn
          (setq-local dired-dotfiles-show-p nil)
          (dired-mark-files-regexp "^\\.")
          (dired-do-kill-lines))
      (revert-buffer)
      (setq-local dired-dotfiles-show-p t)))
  :custom
  (dired-isearch-filenames 'dwim)
  (dired-create-destination-dirs 'ask)
  (dired-vc-rename-file t))

(use-package dired-x
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :custom
  (dired-omit-verbose nil)
  (dired-omit-files (rx (or ".DS_Store"
                            ".cache"
                            ".vscode"
                            ".ccls-cache" ".clangd")
                        string-end))
  ;; Dont prompt about killing buffer visiting delete file
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-guess-shell-alist-user `((,(rx "."
                                        (or
                                         ;; Videos
                                         "mp4" "avi" "mkv" "flv" "ogv" "ogg" "mov"
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

;; Make dired narrow-able
(use-package dired-narrow
  :ensure t
  :after dired evil-collection
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "s" 'dired-narrow-regexp)
  :custom
  (dired-narrow-exit-when-one-left t)
  (dired-narrow-enable-blinking t)
  (dired-narrow-blink-time 0.3))

(provide 'init-dired)

;;; init-dired.el ends here
