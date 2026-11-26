;;; init-org.el --- Org mode configurations -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)

(use-package org
  :ensure nil
  :hook (org-mode . visual-line-mode)
  :custom
  (org-directory "~/.org/")
  (org-default-notes-file (expand-file-name "notes.org" org-directory))
  ;; prettify
  (org-startup-indented t)
  (org-fontify-whole-heading-line t)
  (org-fontify-quote-and-verse-blocks t)
  (org-list-demote-modify-bullet '(("+" . "-") ("1." . "a.") ("-" . "+")))
  ;; image
  (org-image-actual-width nil)
  ;; more user-friendly
  (org-imenu-depth 4)
  (org-clone-delete-id t)
  (org-use-sub-superscripts '{})
  (org-yank-adjusted-subtrees t)
  (org-ctrl-k-protect-subtree 'error)
  (org-fold-catch-invisible-edits 'show-and-error)
  ;; todo
  (org-todo-keywords '((sequence "TODO(t)" "HOLD(h!)" "WIP(i!)" "WAIT(w!)" "|" "DONE(d!)" "CANCELLED(c@/!)")))
  (org-todo-keyword-faces '(("TODO"       :foreground "#7c7c75" :weight bold)
                            ("HOLD"       :foreground "#feb24c" :weight bold)
                            ("WIP"        :foreground "#0098dd" :weight bold)
                            ("WAIT"       :foreground "#9f7efe" :weight bold)
                            ("DONE"       :foreground "#50a14f" :weight bold)
                            ("CANCELLED"  :foreground "#ff6480" :weight bold)))
  (org-use-fast-todo-selection 'expert)
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)
  (org-priority-faces '((?A :foreground "red")
                        (?B :foreground "orange")
                        (?C :foreground "yellow")))
  (org-global-properties '(("EFFORT_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00")
                           ("APPT_WARNTIME_ALL" . "0 5 10 15 20 25 30 45 60")
                           ("STYLE_ALL" . "habit")))
  (org-columns-default-format "%25ITEM %TODO %SCHEDULED %DEADLINE %3PRIORITY %TAGS %CLOCKSUM %EFFORT{:}")
  ;; log
  (org-log-into-drawer t)
  ;; refile
  (org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  ;; goto. We use minibuffer to filter instead of isearch.
  (org-goto-auto-isearch nil)
  (org-goto-interface 'outline-path-completion)
  ;; tags, e.g. #+TAGS: keyword in your file
  (org-use-fast-tag-selection t)
  (org-fast-tag-selection-single-key t)
  ;; archive
  (org-archive-location "%s_archive::datetree/")
  ;; id
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  ;; abbreviation for url
  (org-link-abbrev-alist '(("GitHub" . "https://github.com/")
                           ("GitLab" . "https://gitlab.com/")
                           ("Google" . "https://google.com/search?q=")
                           ("RFCs"   . "https://www.rfc-editor.org/rfc/")
                           ("LWN"    . "https://lwn.net/Articles/")
                           ("WG21"   . "https://wg21.link/")))
  ;; babel
  (org-confirm-babel-evaluate nil)
  (org-babel-load-languages '((C          . t)
                              (dot        . t)
                              (emacs-lisp . t)
                              (eshell     . t)
                              (python     . t)
                              (shell      . t)))
  ;; export
  (org-export-with-sub-superscripts '{}))

;; Keep track of tasks
(use-package org-agenda
  :ensure nil
  :hook (org-agenda-finalize . org-agenda-refresh-appts)
  :config
  (defun org-agenda-refresh-appts ()
    "Rebuild appointment reminders from Org agenda files."
    (org-agenda-to-appt t))
  ;; Rebuild the appointment list from Org every 5 minutes.
  (run-at-time t 300 #'org-agenda-refresh-appts)
  (shut-up! #'org-agenda-to-appt)
  :custom
  (org-agenda-files (list (expand-file-name "tasks.org" org-directory)))
  (org-agenda-diary-file (expand-file-name "diary.org" org-directory))
  (org-agenda-insert-diary-extract-time t)
  (org-agenda-inhibit-startup t)
  (org-agenda-time-leading-zero t)
  (org-agenda-columns-add-appointments-to-effort-sum t))

;; Write codes in org-mode
(use-package org-src
  :ensure nil
  :bind (:map org-src-mode-map
         ;; consistent with separedit/magit
         ("C-c C-c" . org-edit-src-exit))
  :custom
  (org-src-content-indentation 0))

;; D2-inspired SVG styling for Graphviz dot blocks
(use-package ob-dot
  :ensure nil
  :defer t
  :config
  (setq org-babel-default-header-args:dot
        `((:results . "file")
          (:exports . "both")
          (:cmdline . ,(string-join
                        '("-Tsvg"
                          ;; White canvas + padding for GitHub light/dark themes
                          "-Gbgcolor=#ffffff"
                          "-Gpad=0.3"
                          "-Gnodesep=0.5"
                          "-Granksep=0.6"
                          "-Gsplines=polyline"
                          "-Gfontname=sans-serif"
                          "-Gfontsize=13"
                          "-Gfontcolor=#1e293b"
                          ;; D2-style rounded cards with generous padding for CJK
                          "-Nshape=box"
                          "-Nstyle=rounded,filled"
                          "-Nmargin=0.22,0.12"
                          "-Nfillcolor=#edf2ff"
                          "-Ncolor=#4a6cf7"
                          "-Npenwidth=1.2"
                          "-Nfontname=sans-serif"
                          "-Nfontsize=13"
                          "-Nfontcolor=#1e293b"
                          ;; Crisp muted connectors
                          "-Ecolor=#64748b"
                          "-Epenwidth=1.2"
                          "-Earrowhead=vee"
                          "-Earrowsize=0.75"
                          "-Efontname=sans-serif"
                          "-Efontsize=11"
                          "-Efontcolor=#475569")
                        " ")))))

;; Create structured information quickly
(use-package org-capture
  :ensure nil
  :hook (org-capture-mode . org-capture-setup)
  :config
  (with-no-warnings
    (defun org-capture-setup ()
      (setq-local org-complete-tags-always-offer-all-agenda-tags t)))
  :custom
  (org-capture-use-agenda-date t)
  (org-capture-templates '(;; Tasks
                           ("t" "Tasks")
                           ("tt" "Today" entry (file+olp+datetree "tasks.org")
                            "* TODO %?%^{EFFORT}p"
                            :prepend t
                            :empty-lines 1)
                           ("ti" "Inbox" entry (file+headline "tasks.org" "Inbox")
                            "* TODO %?\n%i"
                            :empty-lines 1)
                           ("tm" "Mail" entry (file+headline "tasks.org" "Inbox")
                            "* TODO %^{type|reply to|contact} %^{recipient} about %^{subject} :MAIL:"
                            :empty-lines 1)
                           ;; Diary & Notes
                           ("d" "Diary" entry (file+olp+datetree "diary.org")
                            "* %U %?\n%i"
                            :empty-lines 1)
                           ("n" "Note" entry (file+headline "notes.org" "Notes")
                            "* %? %^g\n%i"
                            :empty-lines 1))))

(provide 'init-org)
;;; init-org.el ends here
