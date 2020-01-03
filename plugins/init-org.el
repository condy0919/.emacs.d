;;; init-org.el --- Org mode configurations

;;; Commentary:
;;

;;; Code:

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

(provide 'init-org)

;;; init-org.el ends here
