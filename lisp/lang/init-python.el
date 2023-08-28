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
  :commands pyvenv-deactivate pyvenv-activate
  :config
  (defun pyrightconfig-write ()
    "Write a `pyrightconfig.json' file at the root of a project with
`venvPath` and `venv`."
    (let* ((vp (string-trim-right pyvenv-virtual-env "/"))
           (root (file-name-directory vp))
           (venv (file-name-base vp))
           (out-file (expand-file-name "pyrightconfig.json" root)))
      (with-temp-file out-file
        (insert (json-encode (list :venvPath root
                                   :venv venv))))
      (message "Configured `%s` to use environment `%s`" out-file pyvenv-virtual-env)))

  (add-hook 'pyvenv-post-activate-hooks #'pyrightconfig-write))

(provide 'init-python)
;;; init-python.el ends here
