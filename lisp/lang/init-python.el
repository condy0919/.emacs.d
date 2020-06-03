;;; init-python.el --- Python -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-mode)
  :hook (inferior-python-mode . my/buffer-auto-close)
  :custom
  (python-indent-guess-indent-offset-verbose nil))

(provide 'init-python)
;;; init-python.el ends here
