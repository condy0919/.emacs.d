;;; init-dev.el --- Programming development -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'init-macros)

;; Compilation Mode
(use-package compile
  :ensure nil
  :hook (compilation-filter . ansi-color-compilation-filter)
  :custom
  (compilation-always-kill t)
  (compilation-scroll-output t)
  ;; Save all buffers on M-x `compile'
  (compilation-ask-about-save nil))

;; The unified debugger
(use-package gud
  :ensure nil
  :hook (gud-mode . gud-tooltip-mode)
  :custom
  (gud-highlight-current-line t))

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
  (gdb-debuginfod-enable-setting nil)
  (gdb-restore-window-configuration-after-quit t))

;; #number can be clickable.
(use-package bug-reference
  :ensure nil
  :bind (:map bug-reference-map
         ("C-c C-o" . bug-reference-push-button)))

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
         ("C-c t o" . hl-todo-occur)
         ("C-c t s" . hl-todo-rgrep)))

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
  (projectile-kill-buffers-filter 'kill-only-files)
  ;; Ignore uninteresting files. It has no effect when using alien mode.
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
  :hook ((xref-after-return xref-after-jump) . recenter)
  :custom
  ;; Emacs 28+
  ;;
  ;; `project-find-regexp' can be faster when setting `xref-search-program' to
  ;;  `ripgrep'.
  (xref-search-program (cond ((executable-find "rg") 'ripgrep)
                             ((executable-find "ugrep") 'ugrep)
                             (t 'grep)))
  (xref-history-storage 'xref-window-local-history)
  (xref-show-xrefs-function #'xref-show-definitions-completing-read)
  (xref-show-definitions-function #'xref-show-definitions-completing-read))

;; Jump to definition, used as a fallback of lsp-find-definition
(use-package dumb-jump
  :ensure t
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
         ("C-c c J" . citre-query-jump)
         ("C-c c /" . citre-jump-to-reference)
         ("C-c c ?" . citre-query-jump-to-reference)
         ("C-c c u" . citre-update-this-tags-file)
         ("C-c c g" . citre-global-update-database))
  :custom
  (citre-enable-capf-integration nil)
  (citre-auto-enable-citre-mode-modes '(prog-mode)))

;; Browse devdocs.io
(use-package devdocs
  :ensure t
  :bind ("C-c b" . devdocs-lookup)
  :config
  (add-to-list 'completion-category-overrides '(devdocs (styles . (flex)))))

;; Hiding structured data
;;
;; zm hide-all
;; zr show-all
;; za toggle-fold
;; zo show-block
;; zc hide-block
(use-package hideshow
  :ensure nil
  :hook (prog-mode . hs-minor-mode)
  :custom
  (hs-show-indicators t)
  (hs-display-lines-hidden t)
  (hs-indicator-type (if (display-graphic-p) 'fringe 'margin))
  (hs-hide-comments-when-hiding-all nil))

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
  :custom
  (graphviz-dot-indent-width 2))

;; Syntax highlighting for systemd files
(use-package conf-mode
  :ensure nil
  :mode ((rx "."
             (or "automount" "busname" "link" "mount" "netdev" "network"
                 "path" "service" "slice" "socket" "swap" "target" "timer")
             string-end) . conf-toml-mode))

(use-package treesit
  :ensure nil
  :config
  (setq treesit-language-source-alist
        '((c . ("https://github.com/tree-sitter/tree-sitter-c"))
          (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (json . ("https://github.com/tree-sitter/tree-sitter-json")))))

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
