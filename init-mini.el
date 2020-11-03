;;; init-mini.el --- The minimal configuration -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Emacs minimal configuration for debugging.
;;

;;; Code:

(require 'package)

(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

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

;; Bootstrap `quelpa'.
(use-package quelpa
  :ensure t
  :custom
  (quelpa-update-melpa-p nil)
  (quelpa-self-upgrade-p nil)
  (quelpa-checkout-melpa-p nil))

(use-package quelpa-use-package
  :ensure t
  :config
  (quelpa-use-package-activate-advice)
  :custom
  (quelpa-use-package-inhibit-loading-quelpa t))

(setq debug-on-error t)
(setq-default lexical-binding t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/lang" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'init-base)

;; Incremental complete in minibuffer
(use-package icomplete
  :ensure nil
  :hook (emacs-startup . icomplete-mode)
  :custom
  (icomplete-hide-common-prefix nil)
  (icomplete-separator "\n")
  (icomplete-show-matches-on-no-input nil))

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init-mini)
;;; init-mini.el ends here
