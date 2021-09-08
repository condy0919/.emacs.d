;;; init-macros.el --- core macros  -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defmacro set-company-backends-for! (mode &rest backends)
  "Set `company-backends' for MODE with BACKENDS."
  `(add-hook (intern (format "%s-hook" ',mode))
             (lambda ()
               (company-mode +1)
               (setq-local company-backends ',backends))))

(defmacro shut-up! (func)
  "Silence FUNC."
  `(advice-add ,func :around
               (defun ,(intern (format "shut-up-%s" func)) (f &rest args)
                 (let ((inhibit-message t))
                   (ignore-errors (apply f args))))))

(defmacro lazy! (&rest body)
  "Delay the evaluation of BODY."
  `(lambda ()
     ,@body))

(provide 'init-macros)

;;; init-macros.el ends here
