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

;; Quickly insert url
(use-package quickurl
  :ensure nil
  :bind ("C-c i q" . #'quickurl-prefix-map)
  :config
  (defvar quickurl-prefix-map (make-sparse-keymap))
  (define-prefix-command 'quickurl-prefix-map)
  (define-key quickurl-prefix-map "q" 'quickurl)
  (define-key quickurl-prefix-map "l" 'quickurl-list)
  (define-key quickurl-prefix-map "i" 'quickurl-ask)
  (define-key quickurl-prefix-map "e" 'quickurl-edit-urls)
  (define-key quickurl-prefix-map "a" 'quickurl-add-url)
  (define-key quickurl-prefix-map "b" 'quickurl-browse-url-ask)
  :custom
  (quickurl-format-function #'quickurl-url-url))

(provide 'init-insert)
;;; init-insert.el ends here
