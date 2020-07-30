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
  (evil-collection-outline-bind-tab-p t)
  (evil-collection-term-sync-state-and-mode-p nil)
  (evil-collection-setup-minibuffer nil)
  (evil-collection-setup-debugger-keys nil))

(use-package evil-leader
  :ensure t
  :hook (after-init . global-evil-leader-mode)
  :config
  (setq evil-leader/leader "SPC")

  ;; prefix: <Leader> f, file
  (evil-leader/set-key
    "fj" 'dired-jump
    "fJ" 'dired-jump-other-window
    "ff" 'find-file
    "fF" 'find-file-other-window
    "fo" 'counsel-find-file-extern
    "fD" 'my/delete-current-file
    "fC" 'my/copy-current-file
    "fy" 'my/copy-current-filename
    "fR" 'my/rename-current-file
    "fr" 'counsel-recentf
    "fl" 'find-file-literally)

  ;; prefix: <Leader> b, buffer
  (evil-leader/set-key
    "bp" 'previous-buffer
    "bn" 'next-buffer
    "bb" 'switch-to-buffer
    "bB" 'switch-to-buffer-other-window
    "bi" 'list-buffers
    "by" 'my/copy-current-buffer-name)

  ;; prefix: <Leader> b, bookmark
  (evil-leader/set-key
    "bm" 'bookmark-set
    "bd" 'bookmark-delete
    "bj" 'bookmark-jump
    "bJ" 'bookmark-jump-other-window
    "bl" 'bookmark-bmenu-list
    "bs" 'bookmark-save)

  ;; prefix: <Leader> c, code
  (evil-leader/set-key
    "cc" 'compile
    "cC" 'recompile)

  ;; prefix: <Leader> w, window
  (evil-leader/set-key
    "w" 'evil-window-map)
  (evil-leader/set-key
    "wu" 'my/transient-winner-undo
    "w-" 'split-window-vertically
    "w/" 'split-window-horizontally)

  ;; prefix: <Leader> s, search
  (evil-leader/set-key
    "sa" 'swiper-all
    "sb" 'swiper
    "sg" 'counsel-rg
    "si" 'imenu
    "sj" 'evil-show-jumps
    "sr" 'evil-show-marks
    "ss" 'swiper-isearch
    "sS" 'swiper-isearch-thing-at-point)

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
    "pC" 'projectile-configure-project
    "pP" 'projectile-test-project
    "pa" 'projectile-find-other-file
    "pA" 'projectile-find-other-file-other-window
    "pb" 'projectile-switch-to-buffer
    "pB" 'projectile-switch-to-buffer-other-window
    "pc" 'projectile-compile-project
    "pf" 'projectile-find-file
    "pF" 'projectile-find-file-other-window
    "pg" 'projectile-ripgrep
    "pk" 'projectile-kill-buffers
    "po" 'projectile-multi-occur
    "pp" 'projectile-switch-project)

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
      "SPC f" "files"
      "SPC g" "git"
      "SPC i" "insert"
      "SPC o" "open"
      "SPC s" "search"
      "SPC p" "project")
    )
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
