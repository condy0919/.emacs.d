;;; init-minibuffer.el --- Config for minibuffer completion -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package embark
  :ensure t
  :bind (:map minibuffer-local-map
         ("M-o"     . embark-act)
         ("C-c C-o" . embark-export)
         ("C-c C-c" . embark-collect-snapshot)
        (:map minibuffer-local-completion-map
         ("TAB"     . minibuffer-force-complete)
         ("SPC"     . nil)))
  :hook ((minibuffer-setup . embark-collect-completions-after-input)
         (embark-collect-post-revert . resize-embark-collect-completions))
  :config
  (defun resize-embark-collect-completions ()
    "Resize current window to fit buffer or 40% of the frame height."
    (fit-window-to-buffer (get-buffer-window)
                          (floor (* 0.4 (frame-height))) 1))

  ;; Hide the mode line of the Embark live/completions buffers
  ;;
  ;; `shackle' can't customize the window-parameters.
  (add-to-list 'display-buffer-alist
               '("\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 (display-buffer-at-bottom)
                 (window-parameters . ((no-other-window . t)
                                       (mode-line-format . none)))))
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
  (consult-project-root-function #'projectile-project-root))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after embark consult)

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
