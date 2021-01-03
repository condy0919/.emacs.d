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
     (let ((buf (save-window-excursion (,cmd ,@args))))
       (switch-to-buffer-other-window buf))))

(defmacro my/ignore-errors-for (func)
  "Apply FUNC with `ignore-errors' guarded."
  `(define-advice ,func (:around (f &rest args))
     (ignore-errors (apply f args))))

(defmacro my/interactive (fmt &rest args)
  "A function wrapper of `interactive' with FMT and ARGS."
  `(call-interactively (lambda ,args
                         (interactive ,fmt)
                         (list ,@args))))

(provide 'init-macros)

;;; init-macros.el ends here
