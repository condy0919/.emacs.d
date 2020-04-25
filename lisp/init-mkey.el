;;; init-mkey.el --- Keybindings for myself -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Most are copied from `evil-collection'.

;;; Code:

(require 'evil)

(defgroup mkey nil
  "Keybindings for myself."
  :group 'convenience)

(defcustom mkey-enable-modes '(help
                               bm
                               profiler
                               replace ;; occur is in replace.el
                               tar-mode
                               archive-mode
                               dired
                               doc-view
                               quickrun
                               evil-leader)
  "The list of modes which will be evilified."
  :type '(repeat symbol)
  :group 'mkey)

;;;###autoload
(defun mkey-init ()
  "Register the evil bindings for all modes in `mkey-enable-modes'."
  (interactive)
  (dolist (m mkey-enable-modes)
    (with-eval-after-load m
      (funcall (intern (format "mkey-%s-setup" (symbol-name m)))))))

;;;###autoload
(defun mkey-help-setup ()
  "Setup `evil' bindings for `help-mode'."
  (evil-set-initial-state 'help-mode 'normal)
  (evil-define-key 'normal help-mode-map
    ;; motion
    (kbd "<tab>") 'forward-button
    (kbd "<backtab>") 'backward-button

    ;; quit
    "q" 'quit-window))

;;;###autoload
(defun mkey-profiler-setup ()
  "Setup `evil' bindings for `profiler'."
  (evil-set-initial-state 'profiler-report-mode 'normal)
  (evil-define-key 'normal profiler-report-mode-map
    ;; toggle
    (kbd "<tab>") 'profiler-report-toggle-entry
    "+"           'profiler-report-expand-entry
    "-"           'profiler-report-collapse-entry

    ;; sort
    "o" 'profiler-report-ascending-sort
    "O" 'profiler-report-descending-sort

    ;; open
    (kbd "RET") 'profiler-report-find-entry

    ;; quit
    "q" 'quit-window))

;;;###autoload
(defun mkey-replace-setup ()
  "Setup `evil' bindings for `occur'."
  (evil-set-initial-state 'occur-mode 'normal)
  (evil-define-key 'normal occur-mode-map
    ;; like `wdired-mode'
    (kbd "C-c C-e") 'occur-edit-mode

    ;; op
    (kbd "RET") 'occur-mode-goto-occurrence

    "q"             'quit-window)

  (evil-define-key 'normal occur-edit-mode-map
    ;; like `wdired-mode'
    (kbd "C-c C-c") 'occur-cease-edit)
  )

;;;###autoload
(defun mkey-tar-mode-setup ()
  "Setup `evil' bindings for `tar-mode'."
  (evil-set-initial-state 'tar-mode 'normal)
  (evil-define-key 'normal tar-mode-map
    ;; movement
    "j"  'tar-next-line
    "k"  'tar-previous-line
    "gg" 'beginning-of-buffer
    "G"  'end-of-buffer

    ;; op
    "d" 'tar-flag-deleted
    "r" 'tar-rename-entry
    "x" 'tar-expunge

    ;; quit
    "q" 'quit-window)
  )

;;;###autoload
(defun mkey-archive-mode-setup ()
  "Setup `evil' bindings for `archive-mode'."
  (evil-set-initial-state 'archive-mode 'normal)
  (evil-define-key 'normal archive-mode-map
    ;; movement
    "j"  'archive-next-line
    "k"  'archive-previous-line
    "gg" 'beginning-of-buffer
    "G"  'end-of-buffer

    ;; op
    "d" 'archive-flag-deleted
    "r" 'archive-rename-entry
    "x" 'archive-expunge
    (kbd "RET") 'archive-view

    ;; quit
    "q" 'quit-window)
  )

;;;###autoload
(defun mkey-dired-setup ()
  "Setup `evil' bindings for `dired'."
  (evil-define-key 'normal dired-mode-map
    ;; movement
    "j" 'dired-next-line
    "k" 'dired-previous-line
    "gg" 'beginning-of-buffer
    "G" 'end-of-buffer

    ;; quit
    "q" 'quit-window)
  )

;;;###autoload
(defun mkey-doc-view-setup ()
  "Setup `evil' bindings for `doc-view'."
  (evil-set-initial-state 'doc-view 'normal)
  (evil-define-key 'normal doc-view-mode-map
    ;; movement
    "j"  'doc-view-next-line-or-next-page
    "k"  'doc-view-previous-line-or-previous-page
    "gg" 'doc-view-first-page
    "G"  'doc-view-last-page

    ;; zoom
    "+" 'doc-view-enlarge
    "-" 'doc-view-shrink

    ;; op
    "d" 'archive-flag-deleted
    "r" 'archive-rename-entry
    "x" 'archive-expunge
    (kbd "RET") 'archive-view

    ;; quit
    "q" 'quit-window)
  )

;;;###autoload
(defun mkey-quickrun-setup ()
  "Setup `evil' bindings for `quickrun'."
  (evil-define-key 'normal quickrun--mode-map
    "q" 'quit-window))

;;;###autoload
(defun mkey-bm-setup ()
  "Setup `evil' bindings for `bm'."
  (evil-set-initial-state 'bm-show-mode 'normal)
  (evil-define-key 'normal bm-show-mode-map
    ;; movement
    "j" 'bm-show-next
    "k" 'bm-show-prev

    ;; op
    (kbd "RET") 'bm-show-goto-bookmark

    ;; quit
    "q" 'bm-show-quit-window)
  )

;;;###autoload
(defun mkey-evil-leader-setup ()
  "Setup `evil-leader' bindings."
  ;; prefix: <Leader> f, file
  (evil-leader/set-key
    "fj" 'dired-jump
    "fJ" 'dired-jump-other-window
    "ff" 'find-file
    "fF" 'find-file-other-window
    "fd" 'my/delete-current-file
    "fc" 'copy-file
    "fr" 'counsel-recentf
    "fR" 'my/rename-current-file
    "fl" 'find-file-literally
    "fg" 'counsel-rg)

  ;; prefix: <Leader> b, bookmark
  (evil-leader/set-key
    "bb" 'switch-to-buffer
    "bm" 'bookmark-set
    "bd" 'bookmark-delete
    "bj" 'bookmark-jump
    "bJ" 'bookmark-jump-other-window
    "bl" 'bookmark-bmenu-list
    "bs" 'bookmark-save)

  ;; prefix: <Leader> b, bm
  (evil-leader/set-key
    "bp" 'bm-previous
    "bn" 'bm-next
    "bt" 'bm-toggle
    "ba" 'bm-show-all)

  ;; prefix: <Leader> w, window
  (evil-leader/set-key
    "w" 'evil-window-map)
  (evil-leader/set-key
    "w-" 'split-window-vertically
    "w/" 'split-window-horizontally)

  ;; prefix: <Leader> p, projectile
  (evil-leader/set-key
    "pp" 'projectile-switch-project
    "pb" 'projectile-switch-to-buffer
    "pc" 'projectile-compile-project
    "pC" 'projectile-configure-project
    "pP" 'projectile-test-project
    "pa" 'projectile-find-other-file
    "pf" 'projectile-find-file
    "pg" 'projectile-ripgrep)

  ;; prefix: <Leader> a, apps
  (evil-leader/set-key
    "am" 'mu4e
    "ad" 'deft
    "aa" 'org-agenda
    "ac" 'org-capture
    "al" 'org-store-link
    "at" 'org-todo-list)

  ;; prefix: <Leader> o, open
  (evil-leader/set-key
    "ot" 'shell-pop
    "oT" 'vterm
    "oo" 'vterm-other-window)

  ;; frequently used keys
  (evil-leader/set-key
    "q" 'kill-this-buffer
    "i" 'counsel-imenu
    "g" 'counsel-rg)

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
      "SPC o" "open"
      "SPC p" "project")
    )
  )

(provide 'init-mkey)
;;; init-mkey.el ends here
