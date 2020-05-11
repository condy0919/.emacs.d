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

(use-package tab-line
  :ensure nil
  :when (>= emacs-major-version 27)
  :custom-face
  (tab-line ((t (:foreground "brightmagenta" :background "brightblack" :height 0.9 :inherit variable-pitch))))
  (tab-line-tab ((t (:foreground "magenta" :background "brightwhite" :box (:line-width 2 :color "black" :style nil)))))
  (tab-line-tab-inactive ((t (:foreground "brightmagenta" :background "brightblack" :box (:line-width 2 :color "#121212")))))
  (tab-line-tab-current ((t (:inherit tab-line-tab))))
  :custom
  (tab-line-new-button-show nil)
  (tab-line-close-button-show nil)
  (tab-line-separator "")
  (tab-line-exclude-modes '(completion-list-mode
                            ediff-mode
                            vterm-mode))
  )

(provide 'init-ui)

;;; init-ui.el ends here
