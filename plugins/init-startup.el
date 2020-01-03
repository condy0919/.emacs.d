;;; init-startup.el --- The startup dashboard

;;; Commentary:
;;

;;; Code:

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :init
  (setq recentf-max-saved-items 300))

(use-package page-break-lines
  :ensure t)

(use-package dashboard
  :ensure t
  :config
  (setq dashboard-banner-logo-title "Welcome to Emacs Dashboard"
        dashboard-startup-banner 'logo
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-set-init-info t)
  (dashboard-setup-startup-hook))

(provide 'init-startup)

;;; init-startup.el ends here
