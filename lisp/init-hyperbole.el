;;; init-hyperbole.el --- hyperbole -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package hyperbole
  :ensure t
  :custom
  (hmouse-middle-flag nil)
  (inhibit-hsys-org t)
  (inhibit-hyperbole-messaging t)
  ;; role
  (hyrolo-date-format "%Y-%m-%d")
  (hyrolo-google-contacts-flag nil)
  (hyrolo-kill-buffers-after-use t)
  ;; window control
  (hycontrol-invert-mode-line-flag nil)
  (hycontrol-keep-window-flag t))

(provide 'init-hyperbole)
;;; init-hyperbole.el ends here
