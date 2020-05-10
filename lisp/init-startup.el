;;; init-startup.el --- The startup dashboard -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package recentf
  :ensure nil
  :hook ((after-init . recentf-mode)
         (focus-out-hook . (recentf-save-list recentf-cleanup)))
  :custom
  (recentf-max-saved-items 300)
  (recentf-auto-cleanup 'never)
  (recentf-exclude '((expand-file-name package-user-dir)
                     no-littering-var-directory
                     no-littering-etc-directory
                     ".cache"
                     "cache"
                     "recentf"
                     "^/tmp/"
                     "/ssh:"
                     "/su\\(do\\)?:"
                     "^/usr/include/"
                     "bookmarks"
                     "COMMIT_EDITMSG\\'")))

(use-package page-break-lines
  :ensure t
  :hook ((emacs-lisp-mode compilation-mode help-mode) . page-break-lines-mode))

(use-package dashboard
  :ensure t
  :hook ((after-init . dashboard-setup-startup-hook)
         (dashboard-mode . (lambda ()
                             (setq-local global-hl-line-mode nil))))
  :custom-face
  (dashboard-heading ((t (:foreground "#f1fa8c" :weight bold))))
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-init-info t)
  (dashboard-set-navigator t)
  (dashboard-items '((recents . 10)
                     (projects . 5)
                     (bookmarks . 5))))

(provide 'init-startup)

;;; init-startup.el ends here
