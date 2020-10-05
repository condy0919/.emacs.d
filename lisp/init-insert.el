;;; init-insert.el --- Snippets -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(eval-when-compile
  (require 'skeleton))

(define-skeleton insert-date-time
  "Insert current date time with 2 date formats available."
  ""
  > (let ((full (format-time-string "%F %T"))
          (local (format-time-string "%c")))
      (completing-read "datetime: " `(,full ,local) nil t)))

(provide 'init-insert)
;;; init-insert.el ends here
