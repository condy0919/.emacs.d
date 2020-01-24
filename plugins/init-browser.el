;;; init-browser.el --- Edit in Emacs -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package atomic-chrome
  :ensure t
  :config
  :hook (after-init . (lambda ()
                        (atomic-chrome-start-server)))
  :custom
  (atomic-chrome-default-major-mode 'markdown-mode)
  (atomic-chrome-extension-type-list '(ghost-text)) ;; or 'atomic-chrome
  )

(provide 'init-browser)

;;; init-browser.el ends here
