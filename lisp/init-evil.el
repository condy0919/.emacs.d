;;; init-evil.el --- Bring vim back -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'init-core)

(use-package evil
  :ensure t
  :hook (after-init . evil-mode)
  :bind (:map evil-normal-state-map
         ("gs" . evil-avy-goto-char-timer)
         ("go" . evil-avy-goto-word-or-subword-1)
         ("gl" . evil-avy-goto-line))
  :config
  (evil-ex-define-cmd "q[uit]" 'kill-this-buffer)

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
  (evil-disable-insert-state-bindings t)
  ;; j&k operate via visual line
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
  ;; Disable `evil-collection' in certain modes
  (dolist (ig-mode '())
    (setq evil-collection-mode-list (remove ig-mode evil-collection-mode-list)))

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
    ;; file
    "f" '(:ignore t :which-key "file")
    "ff" 'find-file
    "f." 'find-file
    "fF" 'find-file-other-window
    "f/" 'find-file-other-window
    "fg" 'rgrep
    "fj" 'counsel-file-jump
    "fo" 'counsel-find-file-extern
    "fC" 'my/copy-current-file
    "fD" 'my/delete-current-file
    "fy" 'my/copy-current-filename
    "fR" 'my/rename-current-file
    "fr" 'counsel-recentf
    "fl" 'find-file-literally
    "fz" 'counsel-fzf

    ;; dired
    "d" '(:ignore t :which-key "dired")
    "dj" 'dired-jump
    "dJ" 'dired-jump-other-window

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
    "wu" 'my/transient-winner-undo
    "wg" 'hydra-other-window-scroll/body
    "w-" 'split-window-vertically
    "w/" 'split-window-horizontally

    ;; tab
    "t" '(:ignore t :which-key "tab")
    "tb" 'switch-to-buffer-other-tab
    "tc" 'tab-bar-close-tab
    "ti" 'tab-switcher
    "tn" 'tab-bar-new-tab
    "to" 'tab-bar-close-other-tabs
    "tt" 'tab-bar-switch-to-tab
    "tp" 'tab-bar-switch-to-recent-tab
    "tr" 'tab-bar-rename-tab
    "tu" 'my/transient-tab-bar-undo-close-tab

    ;; text
    "x" '(:ignore t :which-key "text")
    "xj" '(:ignore t :which-key "justification")
    "xjc" 'set-justification-center
    "xjf" 'set-justification-full
    "xjl" 'set-justification-left
    "xjn" 'set-justification-none
    "xjr" 'set-justification-right
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
    "sw" 'my/lsp-ivy-workspace-symbol

    ;; insert
    "i" '(:ignore t :which-key "insert")
    "iq" 'quickurl-prefix-map
    "it" 'insert-date-time
    "iu" 'counsel-unicode-char
    "iy" 'clipboard-yank

    ;; git
    "g" '(:ignore t :which-key "git")
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
    "aC" 'calendar
    "aa" 'org-agenda
    "ac" 'org-capture
    "ag" 'gnus
    "ai" 'rcirc
    "aj" 'jblog
    "al" 'org-store-link
    "an" 'newsticker-show-news
    "at" 'org-todo-list

    ;; open
    "o" '(:ignore t :which-key "open")
    "ot" 'my/ansi-term
    "oT" 'my/ansi-term-other-window
    "oe" 'eshell
    "oE" 'my/eshell-other-window
    "os" (when (commandp 'osx-dictionary-search-word-at-point) 'osx-dictionary-search-word-at-point))

  (general-create-definer local-leader-def
    :states 'normal
    :prefix "SPC m")

  (local-leader-def
    :keymaps 'org-mode-map
    "." 'counsel-org-goto
    "/" 'counsel-org-goto-all
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
  :custom
  (general-implicit-kbd t)
  (general-override-auto-enable t))

(use-package evil-surround
  :ensure t
  :hook (after-init . global-evil-surround-mode))

(use-package evil-magit
  :ensure t
  :after evil magit)

(provide 'init-evil)

;;; init-evil.el ends here
