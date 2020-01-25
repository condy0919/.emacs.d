;;; init-browser.el --- Edit in Emacs -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package edit-server
  :ensure t
  :custom
  (edit-server-default-major-mode 'markdown-mode)
  :config
  (edit-server-start))

(provide 'init-browser)

;;; init-browser.el ends here
