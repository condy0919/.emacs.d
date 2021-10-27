;;; init-minibuffer.el --- Config for minibuffer completion -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)
  :custom
  (vertico-sort-function nil))

;; `embark-dwim' invoke actions like `hyperbole'.
(use-package embark
  :ensure t
  :bind (("M-RET"   . embark-either)
         :map minibuffer-local-map
         ("M-o"     . embark-act)
         ("C-c C-c" . embark-export)
         ("C-c C-o" . embark-collect-snapshot))
  :config
  (defun embark-either (&optional arg)
    "Invoke `embark-act' is ARG is non-nil, otherwise invoke
`embark-dwim'."
    (interactive "P")
    (if arg
        (embark-act)
      (embark-dwim)))
  :custom
  (embark-collect-initial-view-alist '((t . list)))
  (embark-collect-live-initial-delay 0.15)
  (embark-collect-live-update-delay 0.15))

(use-package consult
  :ensure t
  :bind (([remap imenu]              . consult-imenu)
         ([remap goto-line]          . consult-goto-line)
         ([remap bookmark-jump]      . consult-bookmark)
         ([remap recentf-open-files] . consult-recent-file)
         ([remap evil-show-marks]    . consult-mark))
  :config
  (with-no-warnings
    (consult-customize consult-ripgrep consult-git-grep consult-grep
                       consult-bookmark
                       consult-recent-file
                       consult-buffer
                       :preview-key nil))
  :custom
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
