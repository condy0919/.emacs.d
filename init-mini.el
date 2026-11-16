;;; init-mini.el --- Built-in-only rescue configuration -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Start with: emacs -Q -l ~/.emacs.d/init-mini.el
;; Keep this independent of packages, init-base and personal customizations.
;;

;;; Code:

(setq debug-on-error t
      use-short-answers t)

;; With -Q -l, `after-init-hook' has already run.  Enable modes directly.
(column-number-mode 1)
(show-paren-mode 1)

;; Incremental complete in minibuffer
(require 'icomplete)
(setq icomplete-prospects-height 10
      icomplete-hide-common-prefix nil
      icomplete-show-matches-on-no-input nil)
(icomplete-vertical-mode 1)

(provide 'init-mini)
;;; init-mini.el ends here
