;;; early-init.el --- Emacs 27+ pre-initialisation config -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; Defer garbage collection further back in the startup process, then restore
;; a practical threshold once startup completes.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024))))

;; Faster to disable these here (before they've been initialized)
;; Enable `menu-bar' on MacOS
(unless (eq system-type 'darwin)
  (push '(menu-bar-lines . 0) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; No eyes distraction
(blink-cursor-mode -1)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

(provide 'early-init)
;;; early-init.el ends here
