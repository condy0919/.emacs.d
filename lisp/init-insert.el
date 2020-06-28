;;; init-insert.el --- Snippets -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'skeleton)

(define-skeleton insert-date-time
  "Insert current date time with 2 date formats available."
  ""
  > (let ((full (format-time-string "%F %T"))
          (local (format-time-string "%c")))
      (completing-read "datetime: " `(,full ,local))))

(define-skeleton insert-mail-signature
  "Insert mail signature in org-mode format."
  ""
  > "Regards,\n\n"
  > "#+begin_signature\n"
  > "-- *" (skeleton-read "Your signature: ") "*\n"
  > "#+end_signature\n")

(provide 'init-insert)
;;; init-insert.el ends here
