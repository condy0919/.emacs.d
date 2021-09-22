;;; init-dev.el --- Programming development -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'init-macros)

;; Compilation Mode
(use-package compile
  :ensure nil
  :hook (compilation-filter . colorize-compilation-buffer)
  :config
  (defun colorize-compilation-buffer ()
    "ANSI coloring in compilation buffers."
    (with-silent-modifications
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :custom
  (compilation-always-kill t)
  (compilation-scroll-output t)
  ;; Save all buffers on M-x `compile'
  (compilation-ask-about-save nil))

;; The unified debugger
;; WIP: upstreaming
(use-package gud
  :ensure nil
  :hook (gud-mode . gud-tooltip-mode))

;; GDB specific config
(use-package gdb-mi
  :ensure nil
  :commands gdb
  :custom
  (gdb-show-main t)
  (gdb-display-io-nopopup t)
  (gdb-show-changed-values t)
  (gdb-delete-out-of-scope t)
  (gdb-use-colon-colon-notation t)
  (gdb-restore-window-configuration-after-quit t))

;; Insert SPDX license header
(use-package spdx
  :ensure t
  :hook (prog-mode . spdx-tempo-setup)
  :custom
  (spdx-ignore-deprecated t))

;; Highlight TODO
(use-package hl-todo
  :ensure t
  :hook (after-init . global-hl-todo-mode)
  :bind (:map hl-todo-mode-map
         ("C-c t p" . hl-todo-previous)
         ("C-c t n" . hl-todo-next)
         ("C-c t i" . hl-todo-insert)
         ("C-c t o" . hl-todo-occur)))

;; Show trailing whitespaces
(use-package whitespace
  :ensure nil
  :hook ((prog-mode markdown-mode conf-mode) . whitespace-mode)
  :custom
  (whitespace-style '(face trailing)))

;; Quickrun codes, including cpp. awesome!
(use-package quickrun
  :ensure t
  :bind ("C-c x" . quickrun)
  :custom
  (quickrun-focus-p nil)
  (quickrun-input-file-extension ".qr"))

;; Project management
(use-package projectile
  :ensure t
  :hook (after-init . projectile-mode)
  :bind (:map projectile-mode-map
         ("C-c p" . projectile-command-map))
  :config
  (dolist (dir '("bazel-bin"
                 "bazel-out"
                 "bazel-testlogs"))
    (add-to-list 'projectile-globally-ignored-directories dir))
  :custom
  (projectile-use-git-grep t)
  (projectile-indexing-method 'alien)
  ;; Ignore uninteresting files. It has no effects when use alien mode.
  (projectile-globally-ignored-files '("TAGS" "tags" ".DS_Store"))
  (projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o" ".swp" ".so" ".a"))
  (projectile-ignored-projects `("~/"
                                 "/tmp/"
                                 "/private/tmp/"
                                 ,package-user-dir)))

;; Lint tool
(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-temp-prefix ".flycheck")
  (flycheck-check-syntax-automatically '(save mode-enabled))
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-indication-mode 'right-fringe))

;; xref
(use-package xref
  :ensure nil
  :init
  ;; On Emacs 28, `xref-search-program' can be set to `ripgrep'.
  ;; `project-find-regexp' benefits from that.
  (when (>= emacs-major-version 28)
    (setq xref-search-program 'ripgrep)
    (setq xref-show-xrefs-function #'xref-show-definitions-completing-read)
    (setq xref-show-definitions-function #'xref-show-definitions-completing-read))
  :hook ((xref-after-return xref-after-jump) . recenter))

;; Jump to definition, used as a fallback of lsp-find-definition
(use-package dumb-jump
  :ensure t
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate t)
  :bind (("M-g j" . dumb-jump-go)
         ("M-g J" . dumb-jump-go-other-window))
  :custom
  (dumb-jump-quiet t)
  (dumb-jump-aggressive t)
  (dumb-jump-selector 'completing-read))

;; A fancy ctags frontend
(use-package citre
  :ensure t
  :init
  ;; Load the prelude.
  (require 'citre-config)
  :bind (("C-c c j" . citre-jump)
         ("C-c c J" . citre-jump-back)
         ("C-c c p" . citre-peek)
         ("C-c c a" . citre-ace-peek)
         ("C-c c u" . citre-update-this-tags-file))
  :custom
  (citre-auto-enable-citre-mode-modes '(prog-mode))
  (citre-prompt-language-for-ctags-command t)
  (citre-ctags-program (executable-find "ctags"))
  (citre-readtags-program (executable-find "readtags")))

;; Browse devdoc.io
(use-package devdocs
  :ensure t
  :bind ("C-c b" . devdocs-lookup)
  :config
  (add-to-list 'completion-category-defaults '(devdocs (styles . (flex)))))

;; Hiding structured data
;;
;; zm hide-all
;; zr show-all
;; za toggle-fold
;; zo show-block
;; zc hide-block
(use-package hideshow
  :ensure nil
  :hook (prog-mode . hs-minor-mode))

;; Antlr mode
(use-package antlr-mode
  :ensure nil
  :mode ("\\.g4\\'" . antlr-mode))

;; XML
(use-package nxml-mode
  :ensure nil
  :mode (("\\.xml\\'" . nxml-mode)
         ("\\.rss\\'" . nxml-mode))
  :custom
  (nxml-slash-auto-complete-flag t)
  (nxml-auto-insert-xml-declaration-flag t))

;; Config files mode
(use-package yaml-mode
  :ensure t
  :mode ("\\.ya?ml\\'" . yaml-mode))

;; The dot-language
(use-package graphviz-dot-mode
  :ensure t
  :mode ("\\.dot\\'" . graphviz-dot-mode)
  :config
  (set-company-backends-for! graphviz-dot-mode company-dabbrev-code company-dabbrev company-graphviz-dot-backend)
  :custom
  (graphviz-dot-indent-width 2))

;; Syntax highlighting for systemd files
(use-package conf-mode
  :ensure nil
  :mode ((rx "."
             (or "automount" "busname" "link" "mount" "netdev" "network"
                 "path" "service" "slice" "socket" "swap" "target" "timer")
             string-end) . conf-toml-mode))

(require 'init-cpp)
(require 'init-rust)
(require 'init-ocaml)
(require 'init-bazel)
(require 'init-haskell)
(require 'init-python)
(require 'init-elisp)
(require 'init-sh)

(provide 'init-dev)

;;; init-dev.el ends here
