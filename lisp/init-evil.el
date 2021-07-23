;;; init-evil.el --- Bring vim back -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)

(use-package evil
  :ensure t
  :init
  (setq evil-disable-insert-state-bindings t)
  :hook (after-init . evil-mode)
  ;; Don't quit Emacs on :q
  :bind ([remap evil-quit] . kill-this-buffer)
  :config
  ;; Install `undo-fu' when necessary
  (when (< emacs-major-version 28)
    (use-package undo-fu
      :ensure t))
  :custom
  ;; undo will never freeze my Emacs
  (evil-undo-system (if (>= emacs-major-version 28) 'undo-redo 'undo-fu))
  ;; Switch to the new window after splitting
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-ex-complete-emacs-commands nil)
  (evil-ex-interactive-search-highlight 'selected-window)
  ;; when `visual-line-mode' enabled, exchange j/k with gj/gk
  (evil-respect-visual-line-mode t)
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-want-fine-undo t)
  (evil-want-C-g-bindings t)
  (evil-want-Y-yank-to-eol t)
  (evil-want-abbrev-expand-on-insert-exit nil)
  (evil-symbol-word-search t))

(use-package evil-collection
  :ensure t
  :hook (evil-mode . evil-collection-init)
  :config
  ;; Keybindings tweaks
  (evil-collection-define-key 'normal 'occur-mode-map
    ;; consistent with ivy
    (kbd "C-c C-e") 'occur-edit-mode)
  :custom
  (evil-collection-calendar-want-org-bindings t)
  (evil-collection-company-use-tng nil)
  (evil-collection-outline-bind-tab-p nil)
  (evil-collection-setup-minibuffer nil)
  (evil-collection-setup-debugger-keys nil))

;; evil leader map
(use-package general
  :ensure t
  :after evil
  :config
  (general-create-definer leader-def
    :states 'normal
    :prefix "SPC"
    :keymaps 'override)

  (leader-def
    ;; SPC is back!
    "SPC" '(my/transient-spc :which-key "SPC")

    ;; file
    "f" '(:ignore t :which-key "file")
    "ff" 'find-file
    "f." 'find-file
    "fF" 'find-file-other-window
    "f/" 'find-file-other-window
    "fg" 'rgrep
    "fj" 'counsel-file-jump
    "fC" 'my/copy-current-file
    "fD" 'my/delete-current-file
    "fy" 'my/copy-current-filename
    "fR" 'my/rename-current-file
    "fr" 'recentf-open-files
    "fl" 'find-file-literally

    ;; dired
    "d" '(:ignore t :which-key "dired")
    "dj" 'dired-jump
    "dJ" 'dired-jump-other-window
    "dM" 'make-directory
    "dC" 'copy-directory
    "dD" 'delete-directory
    "dl" 'list-directory

    ;; buffer & bookmark
    "b" '(:ignore t :which-key "buffmark")
    "bb" 'switch-to-buffer
    "bB" 'switch-to-buffer-other-window
    "bc" 'clone-indirect-buffer
    "bC" 'clone-indirect-buffer-other-window
    "by" 'my/copy-current-buffer-name
    "bv" 'revert-buffer
    "bz" 'bury-buffer
    ;; --------------
    "bm" 'bookmark-set
    "bM" 'bookmark-set-no-overwrite
    "bi" 'bookmark-insert
    "br" 'bookmark-rename
    "bd" 'bookmark-delete
    "bw" 'bookmark-write
    "bj" 'bookmark-jump
    "bJ" 'bookmark-jump-other-window
    "bl" 'bookmark-bmenu-list
    "bs" 'bookmark-save

    ;; code
    "c" '(:ignore t :which-key "code")
    "ca" 'add-change-log-entry-other-window
    "cd" 'rmsbolt-compile
    "cc" 'compile
    "cC" 'recompile
    "ck" 'kill-compilation
    "cx" 'quickrun
    "cX" 'quickrun-shell

    ;; window
    "w" '(:keymap evil-window-map :which-key "window")
    "wx" 'kill-buffer-and-window
    "wu" 'my/transient-tab-bar-history
    "w-" 'split-window-vertically
    "w/" 'split-window-horizontally

    ;; tab
    "t" '(:ignore t :which-key "tab")
    "t9" 'tab-bar-switch-to-last-tab
    "tc" 'tab-bar-close-tab
    "tC" 'tab-bar-close-group-tabs
    "tg" 'tab-bar-change-tab-group
    "ti" 'tab-switcher
    "tn" 'tab-bar-new-tab
    "to" 'tab-bar-close-other-tabs
    "tt" 'tab-bar-switch-to-tab
    "tp" 'tab-bar-switch-to-recent-tab
    "tr" 'tab-bar-rename-tab
    "tu" 'my/transient-tab-bar-undo-close-tab

    ;; text
    "x" '(:ignore t :which-key "text")
    "xj" 'set-justification
    "xw" 'delete-trailing-whitespace
    "x TAB" 'indent-rigidly

    ;; search
    "s" '(:ignore t :which-key "search")
    "ss" 'swiper-isearch
    "sS" 'swiper-isearch-thing-at-point
    "sb" 'swiper-all
    "sB" 'swiper-all-thing-at-point
    "sj" 'evil-show-jumps
    "sm" 'evil-show-marks
    "sr" 'evil-show-registers
    "si" 'imenu
    "sl" 'ivy-resume
    "sg" 'counsel-rg

    ;; git
    "g" '(:ignore t :which-key "git")
    "g." 'magit-file-dispatch
    "gb" 'magit-branch-checkout
    "gB" 'magit-blame-addition
    "gc" 'magit-branch-and-checkout
    "gC" 'magit-commit-create
    "gd" 'magit-diff
    "gf" 'magit-find-file
    "gg" 'magit-status
    "gG" 'magit-status-here
    "gi" 'magit-init
    "gr" 'magit-rebase-interactive

    ;; project
    "p" '(:package projectile :keymap projectile-command-map :which-key "project")

    ;; app
    "a" '(:ignore t :which-key "app")
    "aa" 'org-agenda
    "ac" 'calendar
    "ag" 'gnus
    "ai" 'rcirc
    "aj" 'jblog
    "an" 'newsticker-show-news
    "ap" 'proced

    ;; open
    "o" '(:ignore t :which-key "open")
    "oc" 'org-capture
    "ol" 'org-store-link
    "ot" 'ansi-term
    "oe" 'eshell
    "os" 'shell)

  (general-create-definer local-leader-def
    :states 'normal
    :prefix "SPC m")

  (local-leader-def
    :major-modes '(org-mode)
    :keymaps '(org-mode-map)
    "." 'org-goto
    "a" 'org-archive-subtree
    "d" 'org-deadline
    "e" 'org-set-effort
    "f" 'org-footnote-new
    "l" 'org-lint
    "o" 'org-toggle-ordered-property
    "p" 'org-set-property
    "q" 'org-set-tags-command
    "r" 'org-refile
    "s" 'org-schedule
    "t" 'org-todo
    "T" 'org-todo-list

    "b" '(:ignore t :which-key "babel")
    "bp" 'org-babel-previous-src-block
    "bn" 'org-babel-next-src-block
    "be" 'org-babel-expand-src-block
    "bg" 'org-babel-goto-named-src-block
    "bs" 'org-babel-execute-subtree
    "bb" 'org-babel-execute-buffer
    "bt" 'org-babel-tangle
    "bf" 'org-babel-tangle-file
    "bc" 'org-babel-check-src-block
    "bi" 'org-babel-insert-header-arg
    "bI" 'org-babel-view-src-block-info
    "bk" 'org-babel-remove-result-one-or-many

    "c" '(:ignore t :which-key "clock")
    "cc" 'org-clock-in
    "cC" 'org-clock-out
    "cd" 'org-clock-mark-default-task
    "ce" 'org-clock-modify-effort-estimate
    "cg" 'org-clock-goto
    "cl" 'org-clock-in-last
    "cr" 'org-clock-report
    "cs" 'org-clock-display
    "cx" 'org-clock-cancel
    "c=" 'org-clock-timestamps-up
    "c-" 'org-clock-timestamps-down

    "i" '(:ignore t :which-key "insert")
    "id" 'org-insert-drawer
    "in" 'org-add-note
    "it" 'org-time-stamp-inactive
    "iT" 'org-time-stamp)

  (local-leader-def
    :major-modes '(emacs-lisp-mode lisp-interaction-mode)
    :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "i" 'info-lookup-symbol)
  :custom
  (general-implicit-kbd t)
  (general-override-auto-enable t))

(use-package evil-surround
  :ensure t
  :hook (after-init . global-evil-surround-mode))

(provide 'init-evil)

;;; init-evil.el ends here
