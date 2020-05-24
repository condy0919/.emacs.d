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

(provide 'init-macros)

;;; init-macros.el ends here
