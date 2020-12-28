;;; init-dev.el --- Programming development -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(eval-when-compile
  (require 'init-macros))

;; Compilation Mode
(use-package compile
  :ensure nil
  :hook (compilation-filter . colorize-compilation-buffer)
  :config
  (defun colorize-compilation-buffer ()
    "ANSI coloring in compilation buffers."
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))

  (add-to-list 'compilation-finish-functions 'notify-compilation-result)
  (defun notify-compilation-result (_comp-buffer exit-string)
    "Notify after the compilation is done."
    (if (string-match "^finished" exit-string)
        (notify-send :title "Compilation"
                     :body "Compilation successful :-)"
                     :timeout 5000
                     :urgency 'normal)
      (notify-send :title "Compilation"
                   :body "Compilation failed :-("
                   :timeout 5000
                   :urgency 'critical)))
  :custom
  (compilation-always-kill t)
  (compilation-scroll-output t)
  ;; save all buffers on `compile'
  (compilation-ask-about-save nil))

;; Debugger
(use-package gud
  :ensure nil
  :hook (gud-mode . gud-tooltip-mode)
  :config
  ;; Add color to the current GUD line
  ;; From https://kousik.blogspot.com/2005/10/highlight-current-line-in-gdbemacs.html
  (defconst gud-highlight-face 'secondary-selection)

  (defvar gud-overlay
    (let ((overlay (make-overlay (point) (point))))
      (overlay-put overlay 'face gud-highlight-face)
      overlay)
    "Overlay variable for GUD highlighting.")

  (define-advice gud-display-line (:after (true-file _line))
    "Highlight gud current line."
    (when-let* ((buffer (gud-find-file true-file)))
      (with-current-buffer buffer
        (move-overlay gud-overlay (line-beginning-position) (line-end-position)
                      (current-buffer)))))

  (define-advice gud-kill-buffer-hook (:after nil)
    "Remove highlight overlay."
    (delete-overlay gud-overlay))

  (define-advice gud-sentinel (:after (_1 _2))
    "Remove highlight overlay when user quit gud."
    (delete-overlay gud-overlay)))

;; GDB specific config
(use-package gdb-mi
  :ensure nil
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
  :hook (prog-mode . spdx-tempo-setup))

;; Highlight TODO
(use-package hl-todo
  :ensure t
  :hook (after-init . global-hl-todo-mode)
  :bind (:map hl-todo-mode-map
         ("C-c t p" . hl-todo-previous)
         ("C-c t n" . hl-todo-next)
         ("C-c t o" . hl-todo-occur))
  :config
  (dolist (keyword '("BUG" "ISSUE" "NB"))
    (cl-pushnew `(,keyword . ,(face-foreground 'error)) hl-todo-keyword-faces)))

;; Show trailing whitespaces
(use-package whitespace
  :ensure nil
  :hook ((prog-mode markdown-mode conf-mode) . whitespace-mode)
  :custom
  (whitespace-style '(face trailing)))

;; Change log
(use-package add-log
  :ensure nil
  :custom
  (add-log-keep-changes-together t))

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
  ;; Ignore uninterested files
  (projectile-globally-ignored-files '("TAGS" ".DS_Store"))
  (projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o" ".swp" ".so" ".a"))
  (projectile-ignored-projects `("~/"
                                 "/tmp/"
                                 "/private/tmp/"
                                 ,(file-truename (expand-file-name "elpa" user-emacs-directory)))))

;; Comprehensive ivy integration for projectile
(use-package counsel-projectile
  :ensure t
  :after doct
  :hook (after-init . counsel-projectile-mode)
  :custom
  (counsel-projectile-org-capture-templates
   (doct `(:group
           :empty-lines 1
           :children
           (("Project"
             :keys "p"
             :file "${root}/TODO.org"
             :type entry
             :template (lambda () (concat "* TODO " (when (y-or-n-p "Link? ") "%A\n") "%?"))
             :children (("bug"           :keys "b" :headline "Bugs")
                        ("documentation" :keys "d" :headline "Documentation")
                        ("enhancement"   :keys "e" :headline "Enhancements")
                        ("feature"       :keys "f" :headline "Features")
                        ("optimization"  :keys "o" :headline "Optimizations")
                        ("miscellaneous" :keys "m" :headline "Miscellaneous")
                        ("security"      :keys "s" :headline "Security")))))))
  )

;; Lint tool
(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode)
  :config
  (my/ignore-errors-for flycheck-global-teardown)
  :custom
  (flycheck-temp-prefix ".flycheck")
  (flycheck-check-syntax-automatically '(save mode-enabled))
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-indication-mode 'right-fringe)
  ;; clang/gcc/cppcheck flycheckers never know the include path
  (flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc)))

(use-package flymake
  :ensure nil
  :custom
  (flymake-fringe-indicator-position 'right-fringe))

;; xref
(use-package xref
  :ensure nil
  :when (>= emacs-major-version 28)
  :custom
  ;; `project-find-regexp' benefits from that
  (xref-search-program 'ripgrep)
  (xref-show-xrefs-function #'xref-show-definitions-completing-read)
  (xref-show-definitions-function #'xref-show-definitions-completing-read))

;; ivy enhancement
(when (< emacs-major-version 28)
  (use-package ivy-xref
    :ensure t
    :custom
    (xref-show-xrefs-function #'ivy-xref-show-xrefs)
    (xref-show-definitions-function #'ivy-xref-show-defs)))

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
  (dumb-jump-selector 'ivy)
  (dumb-jump-prefer-searcher 'rg)
  (dumb-jump-disable-obsolete-warnings t))

;; pulse current line
(use-package pulse
  :ensure nil
  :preface
  (defun pulse-region (beg end &rest _)
    "Pulse the current region."
    (pulse-momentary-highlight-region beg end))
  (defun pulse-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))
  (defun recenter-and-pulse (&rest _)
    "Recenter and pulse the current line."
    (recenter)
    (pulse-line))
  :init
  ;; better evil notification
  (advice-add #'pop-tag-mark         :after #'recenter-and-pulse)
  (advice-add #'evil-goto-line       :after #'recenter-and-pulse)
  (advice-add #'evil-goto-mark-line  :after #'recenter-and-pulse)
  (advice-add #'what-cursor-position :after #'pulse-line)
  (advice-add #'evil-window-top      :after #'pulse-line)
  (advice-add #'evil-window-middle   :after #'pulse-line)
  (advice-add #'evil-window-bottom   :after #'pulse-line)
  (advice-add #'evil-yank            :after #'pulse-region)
  :hook ((counsel-grep-post-action
          dumb-jump-after-jump
          bookmark-after-jump
          imenu-after-jump) . recenter-and-pulse)
  :custom-face
  (pulse-highlight-start-face ((t (:inherit highlight))))
  (pulse-highlight-face ((t (:inherit highlight)))))

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
  :config
  (defconst hideshow-folded-face '((t (:inherit 'font-lock-comment-face :box t))))

  (defun hideshow-folded-overlay-fn (ov)
    (when (eq 'code (overlay-get ov 'hs))
      (let* ((nlines (count-lines (overlay-start ov) (overlay-end ov)))
             (info (format " ... [%d] " nlines)))
        (overlay-put ov 'display (propertize info 'face hideshow-folded-face)))))

  (setq hs-set-up-overlay 'hideshow-folded-overlay-fn))

;; Antlr mode
(use-package antlr-mode
  :ensure nil
  :mode ("\\.g4\\'" . antlr-mode))

;; XML
(use-package nxml-mode
  :ensure nil
  :magic "<\\?xml"
  :mode (("\\.xml\\'" . nxml-mode)
         ("\\.rss\\'" . nxml-mode))
  :custom
  (nxml-slash-auto-complete-flag t)
  (nxml-auto-insert-xml-declaration-flag t))

;; Config files mode
(use-package yaml-mode
  :ensure t
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'"  . yaml-mode)))

;; The dot-language
(use-package graphviz-dot-mode
  :ensure t
  :mode (("\\.dot\\'" . graphviz-dot-mode)
         ("\\.gv\\'"  . graphviz-dot-mode))
  :config
  (my/set-company-backends-for graphviz-dot-mode company-graphviz-dot-backend)
  :custom
  (graphviz-dot-indent-width 2))

;; interactive frontend for gnuplot
(use-package gnuplot
  :ensure t
  :mode ("\\.gp\\'" . gnuplot-mode)
  :custom
  (gnuplot-basic-offset 2)
  (gnuplot-inline-image-mode 'inline))

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
