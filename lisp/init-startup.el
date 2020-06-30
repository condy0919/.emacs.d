;;; init-startup.el --- The startup dashboard -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package page-break-lines
  :ensure t
  :hook (after-init . global-page-break-lines-mode))

(use-package all-the-icons
  :ensure t
  :when (display-graphic-p)
  :demand t)

(use-package dashboard
  :ensure t
  :init
  ;; Format: "(icon title help action face prefix suffix)"
  (setq dashboard-navigator-buttons `(((,(if (fboundp 'all-the-icons-octicon) (all-the-icons-octicon "mark-github"     :height 1.0 :v-adjust  0.0) "★")
                                        "GitHub" "Browse" (lambda (&rest _) (browse-url homepage-url)))
                                       (,(if (fboundp 'all-the-icons-octicon) (all-the-icons-octicon "heart"           :height 1.1 :v-adjust  0.0) "♥")
                                        "Stars" "Show stars" (lambda (&rest _) (browse-url stars-url)))
                                       (,(if (fboundp 'all-the-icons-material) (all-the-icons-material "report_problem" :height 1.1 :v-adjust -0.2) "⚑")
                                        "Issue" "Report issue" (lambda (&rest _) (browse-url issue-url)) warning)
                                       (,(if (fboundp 'all-the-icons-material) (all-the-icons-material "update"         :height 1.1 :v-adjust -0.2) "♺")
                                        "Update" "Update packages synchronously" (lambda (&rest _) (auto-package-update-now)) success))))

  :hook ((after-init . dashboard-setup-startup-hook)
         (dashboard-mode . (lambda ()
                             (setq-local global-hl-line-mode nil))))
  :config
  (defconst homepage-url "https://github.com/condy0919/.emacs.d")
  (defconst stars-url (concat homepage-url "/stargazers"))
  (defconst issue-url (concat homepage-url "/issues/new"))
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-init-info t)
  (dashboard-set-navigator t)
  (dashboard-items '((recents   . 10)
                     (projects  . 5)
                     (bookmarks . 5))))

(provide 'init-startup)

;;; init-startup.el ends here
