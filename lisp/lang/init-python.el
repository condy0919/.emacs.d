;;; init-python.el --- Python -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-mode)
  :config
  ;; Keep snake_case whole without Superword's incompatible Evil boundaries.
  (modify-syntax-entry ?_ "w" python-mode-syntax-table)
  :custom
  (python-shell-dedicated 'project)
  (python-indent-guess-indent-offset-verbose nil))

(provide 'init-python)
;;; init-python.el ends here
