;;; init-python.el --- Python -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-mode)
  :custom
  (python-indent-guess-indent-offset-verbose nil))

;; python -m venv ENV_DIR
(use-package pyvenv
  :ensure t
  :commands pyvenv-deactivate pyvenv-deactivate)

(provide 'init-python)
;;; init-python.el ends here
