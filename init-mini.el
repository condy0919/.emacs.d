;;; init-mini.el --- The minimal configuration -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Emacs minimal configuration for debugging.
;;

;;; Code:

(require 'package)

(setq package-archives
      '(("melpa"  . "https://melpa.org/packages/")
        ("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure nil)
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t))
(eval-when-compile
  (require 'use-package))

(setq debug-on-error t)
(setq-default lexical-binding t)

(add-to-list 'load-path (file-name-as-directory (locate-user-emacs-file "lisp")))
(setq custom-file (locate-user-emacs-file "custom.el"))

(require 'init-base)

;; Incremental complete in minibuffer
(use-package icomplete
  :ensure nil
  :hook (emacs-startup . icomplete-mode)
  :custom
  (icomplete-vertical-mode t)
  (icomplete-prospects-height 10)
  (icomplete-hide-common-prefix nil)
  (icomplete-show-matches-on-no-input nil))

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init-mini)
;;; init-mini.el ends here
