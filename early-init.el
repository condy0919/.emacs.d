;;; early-init.el --- Emacs 27+ pre-initialisation config
;;; Commentary:

;;; Code:

(setq package-enable-at-startup nil)

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

(provide 'early-init)
;;; early-init.el ends here
