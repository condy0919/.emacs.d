;;; init-macros.el --- core macros  -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defmacro my/set-company-backends-for (mode &rest backends)
  "Set `company-backends' for MODE with BACKENDS."
  `(add-hook (intern (format "%s-hook" ',mode))
             (lambda ()
               (company-mode +1)
               (setq-local company-backends ',backends))))

(defmacro my/other-windowize-for (cmd &rest args)
  "`other-window' version of CMD ARGS."
  `(defun ,(intern (format "my/%s-other-window" cmd)) (&rest _)
     ,(format "Open a `%s' in a new window." cmd)
     (interactive)
     (other-window-prefix)
     (,cmd ,@args)))

(provide 'init-macros)

;;; init-macros.el ends here
