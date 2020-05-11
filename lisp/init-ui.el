;;; init-ui.el --- Theme and modeline -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t)
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-irc nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-github nil)
  (doom-modeline-unicode-fallback t)
  :hook (after-init . doom-modeline-mode))

;; Restore windows layout
(use-package winner-mode
  :ensure nil
  :hook (after-init . winner-mode))

(provide 'init-ui)

;;; init-ui.el ends here
