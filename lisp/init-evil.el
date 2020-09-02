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
  :custom
  ;; Switch to the new window after splitting
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-ex-complete-emacs-commands nil)
  (evil-ex-interactive-search-highlight 'selected-window)
  (evil-disable-insert-state-bindings t)
  (evil-insert-skip-empty-lines t)
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
  (evil-collection-term-sync-state-and-mode-p nil)
  (evil-collection-setup-minibuffer nil)
  (evil-collection-setup-debugger-keys nil))

(use-package evil-leader
  :ensure t
  :hook (after-init . global-evil-leader-mode)
  :config
  (setq evil-leader/leader "SPC")

  ;; prefix: <Leader> d, dired
  (evil-leader/set-key
    "dd" 'counsel-fd-dired-jump
    "dj" 'dired-jump
    "dJ" 'dired-jump-other-window)

  ;; prefix: <Leader> f, file
  (evil-leader/set-key
    "ff" 'find-file
    "fF" 'find-file-other-window
    "fj" 'counsel-fd-file-jump
    "fo" 'counsel-find-file-extern
    "fC" 'my/copy-current-file
    "fD" 'my/delete-current-file
    "fy" 'my/copy-current-filename
    "fR" 'my/rename-current-file
    "fr" 'counsel-recentf
    "fl" 'find-file-literally)

  ;; prefix: <Leader> b, buffer
  (evil-leader/set-key
    "bb" 'switch-to-buffer
    "bB" 'switch-to-buffer-other-window
    "by" 'my/copy-current-buffer-name)

  ;; prefix: <Leader> b, bookmark
  (evil-leader/set-key
    "bm" 'bookmark-set
    "bM" 'bookmark-set-no-overwrite
    "bi" 'bookmark-insert
    "br" 'bookmark-rename
    "bd" 'bookmark-delete
    "bw" 'bookmark-write
    "bj" 'bookmark-jump
    "bJ" 'bookmark-jump-other-window
    "bl" 'bookmark-bmenu-list
    "bs" 'bookmark-save)

  ;; prefix: <Leader> c, code
  (evil-leader/set-key
    "cd" 'rmsbolt-compile
    "cc" 'compile
    "cC" 'recompile
    "cx" 'quickrun)

  ;; prefix: <Leader> w, window
  (evil-leader/set-key
    "w" 'evil-window-map)
  (evil-leader/set-key
    "wdj" 'windmove-delete-down
    "wdk" 'windmove-delete-up
    "wdh" 'windmove-delete-left
    "wdl" 'windmove-delete-right
    "wu" 'my/transient-winner-undo
    "wg" 'my/transient-other-window-nav
    "w-" 'split-window-vertically
    "w/" 'split-window-horizontally)

  ;; prefix: <Leader> t, tab
  (evil-leader/set-key
    "tc" 'tab-bar-close-tab
    "ti" 'tab-switcher
    "tn" 'tab-bar-new-tab
    "to" 'tab-bar-close-other-tabs
    "tr" 'tab-bar-rename-tab-by-name
    "tt" 'tab-bar-select-tab-by-name
    "tu" 'tab-bar-undo-close-tab)

  ;; prefix: <Leader> s, search
  (evil-leader/set-key
    "sa" 'swiper-all
    "sb" 'swiper
    "sg" 'counsel-rg
    "si" 'imenu
    "sj" 'evil-show-jumps
    "sr" 'evil-show-marks
    "ss" 'swiper-isearch
    "sS" 'swiper-isearch-thing-at-point
    "sw" 'my/lsp-ivy-workspace-symbol)

  ;; prefix: <Leader> i, insert
  (evil-leader/set-key
    "iq" 'quickurl-prefix-map
    "is" 'insert-mail-signature
    "it" 'insert-date-time)

  ;; prefix: <Leader> g, git
  (evil-leader/set-key
    "gb" 'magit-branch-checkout
    "gB" 'magit-blame-addition
    "gc" 'magit-branch-and-checkout
    "gC" 'magit-commit-create
    "gd" 'magit-diff
    "gf" 'magit-find-file
    "gg" 'magit-status
    "gG" 'magit-status-here
    "gi" 'magit-init
    "gr" 'magit-rebase-interactive)

  ;; prefix: <Leader> p, projectile
  (evil-leader/set-key
    "p" 'projectile-command-map)

  ;; prefix: <Leader> a, apps
  (evil-leader/set-key
    "am" 'mu4e
    "ag" 'gnus
    "an" 'elfeed
    "ad" 'deft
    "ae" 'elpher
    "aj" 'jblog
    "aa" 'org-agenda
    "ac" 'org-capture
    "aC" 'calendar
    "al" 'org-store-link
    "at" 'org-todo-list)

  ;; prefix: <Leader> o, open
  (evil-leader/set-key
    "ot" 'vterm
    "oT" 'vterm-other-window
    "oe" 'eshell
    "oE" 'my/eshell-other-window)
  (when (commandp 'osx-dictionary-search-word-at-point)
    (evil-leader/set-key
      "os" 'osx-dictionary-search-word-at-point))

  ;; org-mode <Leader> m
  ;; Copy from doom-emacs
  (evil-leader/set-key-for-mode 'org-mode
    "m'" 'org-edit-special
    "m," 'org-switchb
    "m." 'counsel-org-goto
    "m/" 'counsel-org-goto-all
    "mA" 'org-archive-subtree
    "md" 'org-deadline
    "me" 'org-export-dispatch
    "mf" 'org-footnote-new
    "mh" 'org-toggle-heading
    "mi" 'org-toggle-item
    "mI" 'org-toggle-inline-images
    "mn" 'org-store-link
    "mo" 'org-set-property
    "mp" 'org-priority
    "mq" 'org-set-tags-command
    "ms" 'org-schedule
    "mt" 'org-todo
    "mT" 'org-todo-list
    "maa" 'org-attach
    "mad" 'org-attach-delete-one
    "maD" 'org-attach-delete-all
    "man" 'org-attach-new
    "mao" 'org-attach-open
    "maO" 'org-attach-open-in-emacs
    "mar" 'org-attach-reveal
    "maR" 'org-attach-reveal-in-emacs
    "mau" 'org-attach-url
    "mas" 'org-attach-set-directory
    "maS" 'org-attach-sync
    "mb-" 'org-table-insert-hline
    "mba" 'org-table-align
    "mbc" 'org-table-create-or-convert-from-region
    "mbe" 'org-table-edit-field
    "mbh" 'org-table-field-info
    "mcc" 'org-clock-in
    "mcC" 'org-clock-out
    "mcd" 'org-clock-mark-default-task
    "mce" 'org-clock-modify-effort-estimate
    "mcE" 'org-set-effort
    "mcl" 'org-clock-in-last
    "mcg" 'org-clock-goto
    "mcG" (lambda () (org-clock-goto 'select))
    "mcr" 'org-clock-report
    "mcx" 'org-clock-cancel
    "mc=" 'org-clock-timestamps-up
    "mc-" 'org-clock-timestamps-down
    "mgg" 'counsel-org-goto
    "mgG" 'counsel-org-goto-all
    "mgc" 'org-clock-goto
    "mgC" (lambda () (org-clock-goto 'select))
    "mgi" 'org-id-goto
    "mgr" 'org-refile-goto-last-stored
    "mgx" 'org-capture-goto-last-stored
    "mll" 'org-insert-link
    "mlL" 'org-insert-all-links
    "mls" 'org-store-link
    "mlS" 'org-insert-last-stored-link
    "mli" 'org-id-store-link
    "mr" 'org-refile)

  ;; org-agenda-mode <Leader> m
  ;; Copy from doom-emacs
  (evil-leader/set-key-for-mode 'org-agenda-mode
    "md" 'org-agenda-deadline
    "mcc" 'org-agenda-clock-in
    "mcC" 'org-agenda-clock-out
    "mcg" 'org-agenda-goto
    "mcr" 'org-agenda-clockreport-mode
    "mcs" 'org-agenda-show-clocking-issues
    "mcx" 'org-agenda-clock-cancel
    "mq" 'org-agenda-set-tags
    "mr" 'org-agenda-refile
    "ms" 'org-agenda-schedule
    "mt" 'org-agenda-todo)

  ;; Replace with correct prefix names
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements
      "SPC a" "apps"
      "SPC b" "bookmark"
      "SPC c" "code"
      "SPC d" "dired"
      "SPC f" "files"
      "SPC g" "git"
      "SPC i" "insert"
      "SPC o" "open"
      "SPC s" "search"
      "SPC t" "tabs"
      "SPC p" "project"))
  )

(use-package evil-nerd-commenter
  :ensure t
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))

(use-package evil-surround
  :ensure t
  :hook (after-init . global-evil-surround-mode))

(use-package evil-magit
  :ensure t
  :after evil magit)

(provide 'init-evil)

;;; init-evil.el ends here
