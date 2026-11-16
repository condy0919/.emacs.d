;;; init-python.el --- Python -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-mode)
  :hook (python-mode . superword-mode)
  :custom
  (python-shell-dedicated 'project)
  (python-indent-guess-indent-offset-verbose nil))

(provide 'init-python)
;;; init-python.el ends here
