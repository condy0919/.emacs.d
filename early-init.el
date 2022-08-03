;;; early-init.el --- Emacs 27+ pre-initialisation config -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

(unless (or (daemonp) noninteractive)
  ;; Keep a ref to the actual file-name-handler
  (let ((default-file-name-handler-alist file-name-handler-alist))
    ;; Set the file-name-handler to nil (because regexing is cpu intensive)
    (setq file-name-handler-alist nil)
    ;; Reset file-name-handler-alist after initialization
    (add-hook 'emacs-startup-hook
              (lambda ()
                ;; Merge instead of overwrite because there may have bene
                ;; changes to `file-name-handler-alist' since startup we want to
                ;; preserve.
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist
                                           default-file-name-handler-alist)))))))

(provide 'early-init)
;;; early-init.el ends here
