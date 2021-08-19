;;; init-minibuffer.el --- Config for minibuffer completion -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package embark
  :ensure t
  :bind (:map minibuffer-local-map
         ("M-o"     . embark-act)
         ("C-c C-o" . embark-export)
         ("C-c C-c" . embark-collect-snapshot))
  :hook ((minibuffer-setup . embark-collect-completions-after-input)
         (embark-collect-post-revert . resize-embark-collect-completions))
  :config
  (defun resize-embark-collect-completions ()
    "Resize current window to fit buffer or 40% of the frame height."
    (fit-window-to-buffer (get-buffer-window)
                          (floor (* 0.4 (frame-height))) 1))

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-parameters . ((no-other-window . t)
                                       (mode-line-format . none)))))
  :custom
  (embark-collect-initial-view-alist '((t . list))))

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
