;;; init-macros.el --- core macros  -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defmacro my/set-company-backends-for (mode &rest backends)
  "Set `company-backends' for MODE with BACKENDS."
  (let ((funcname (intern (format "my/company-%s" mode)))
        (hookname (intern (format "%s-hook" mode))))
    `(progn
       (defun ,funcname ()
         (company-mode +1)
         (setq-local company-backends ',backends))
       (add-hook ',hookname ',funcname))))

(defmacro my/other-windowize-for (cmd &rest args)
  "`other-window' version of CMD ARGS."
  `(defun ,(intern (format "my/%s-other-window" cmd)) (&rest _)
     ,(format "Open a `%s' in a new window." cmd)
     (interactive)
     (let ((buf (,cmd ,@args)))
       (switch-to-buffer (other-buffer buf))
       (switch-to-buffer-other-window buf)))
  )

(defmacro my/ignore-errors-for (func)
  "Apply FUNC with `ignore-errors' guarded."
  `(define-advice ,func (:around (f &rest args))
     (ignore-errors (apply f args))))

(provide 'init-macros)

;;; init-macros.el ends here
