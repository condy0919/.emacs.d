;;; init.el --- The main entry for emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, and then reset it later
;; using a hook.
;;
;; It's reset by the `gcmh' module.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(unless (or (daemonp) noninteractive)
  ;; Keep a ref to the actual file-name-handler
  (let ((default-file-name-handler-alist file-name-handler-alist))
    ;; Set the file-name-handler to nil (because regexing is cpu intensive)
    (setq file-name-handler-alist nil)
    ;; Reset file-name-handler-alist after initialization
    (add-hook 'emacs-startup-hook
              (lambda ()
                (setq file-name-handler-alist default-file-name-handler-alist)))))

;; Increase how much is read from processes in a single chunk (default is 4kb).
;; `lsp-mode' gains a much
(setq read-process-output-max (* 1024 1024))

(require 'package)
(setq package-archives
      '(("gnu"    . "https://mirrors.cloud.tencent.com/elpa/gnu/")
        ("melpa"  . "https://mirrors.cloud.tencent.com/elpa/melpa/")
        ("nongnu" . "https://mirrors.cloud.tencent.com/elpa/nongnu/")))

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
(require 'init-utils)
(require 'init-ui)
(require 'init-tools)
(require 'init-evil)
(require 'init-lsp)
(require 'init-git)
(require 'init-dev)
(require 'init-dired)
(require 'init-hydra)
(require 'init-ivy)

;; standalone apps
(require 'init-org)
(require 'init-text)
(require 'init-mail)
(require 'init-shell)
(require 'init-spell)
(require 'init-reader)

;; MacOS specific
(when (eq system-type 'darwin)
  (require 'init-osx))

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)

;;; init.el ends here
