;;; init-minibuffer.el --- Config for minibuffer completion -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)
  :custom
  (vertico-sort-function nil))

(use-package embark
  :ensure t
  :bind (:map minibuffer-local-map
         ("M-o"     . embark-act)
         ("C-c C-o" . embark-export)
         ("C-c C-c" . embark-collect-snapshot))
  :config
  (setq prefix-help-command #'embark-prefix-help-command)
  :custom
  (embark-collect-initial-view-alist '((t . list)))
  (embark-collect-live-initial-delay 0.15)
  (embark-collect-live-update-delay 0.15))

(use-package consult
  :ensure t
  :bind (([remap imenu]              . consult-imenu)
         ([remap bookmark-jump]      . consult-bookmark)
         ([remap recentf-open-files] . consult-recent-file)
         ([remap evil-show-marks]    . consult-mark))
  :custom
  (consult-preview-key nil)
  (consult-fontify-preserve nil)
  (consult-async-min-input 2)
  (consult-async-refresh-delay 0.15)
  (consult-async-input-throttle 0.2)
  (consult-async-input-debounce 0.1)
  (consult-project-root-function #'projectile-project-root))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after embark consult)

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
