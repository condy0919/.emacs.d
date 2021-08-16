;;; init-org.el --- Org mode configurations -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)

(use-package org
  :ensure nil
  :hook (org-mode . visual-line-mode)
  :config
  (with-eval-after-load 'evil-collection
    (evil-collection-define-key 'normal 'org-mode-map
      (kbd "<tab>") 'org-cycle
      (kbd "S-<tab>") 'org-shifttab))

  (define-advice org-fast-tag-selection (:around (func &rest args))
    "Hide the modeline in *Org tags* buffer so you can actually see its
content."
    (cl-letf* (((symbol-function 'org-fit-window-to-buffer)
                (lambda (&optional window _max-height _min-height _shrink-only)
                  (when-let (buf (window-buffer window))
                    (with-current-buffer buf
                      (setq mode-line-format nil))))))
      (apply func args)))
  :custom-face
  (org-document-title ((t (:height 1.75 :weight bold))))
  :custom
  (org-directory "~/.org/")
  (org-default-notes-file (expand-file-name "notes.org" org-directory))
  (org-modules '(ol-bibtex ol-gnus ol-info ol-eww org-habit org-protocol))
  ;; prettify
  (org-loop-over-headlines-in-active-region t)
  (org-fontify-todo-headline t)
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-heading-line t)
  (org-hide-macro-markers t)
  (org-hide-emphasis-markers t)
  (org-highlight-latex-and-related '(native script entities))
  (org-pretty-entities t)
  (org-startup-indented t)
  ;; image
  (org-startup-with-inline-images t)
  (org-display-remote-inline-images 'cache)
  (org-list-demote-modify-bullet '(("+" . "-") ("1." . "a.") ("-" . "+")))
  (org-catch-invisible-edits 'smart)
  (org-insert-heading-respect-content t)
  (org-image-actual-width nil)
  (org-imenu-depth 4)
  ;; call C-c C-o explicitly
  (org-return-follows-link nil)
  (org-use-sub-superscripts '{})
  (org-clone-delete-id t)
  (org-yank-adjusted-subtrees t)
  ;; todo
  (org-todo-keywords '((sequence "TODO(t)" "HOLD(h!)" "WIP(i!)" "WAIT(w!)" "|" "DONE(d!)" "CANCELLED(c@/!)")
                       (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f!)")))
  (org-todo-keyword-faces '(("TODO"       :foreground "#7c7c75" :weight bold)
                            ("HOLD"       :foreground "#feb24c" :weight bold)
                            ("WIP"        :foreground "#0098dd" :weight bold)
                            ("WAIT"       :foreground "#9f7efe" :weight bold)
                            ("DONE"       :foreground "#50a14f" :weight bold)
                            ("CANCELLED"  :foreground "#ff6480" :weight bold)
                            ("REPORT"     :foreground "magenta" :weight bold)
                            ("BUG"        :foreground "red"     :weight bold)
                            ("KNOWNCAUSE" :foreground "yellow"  :weight bold)
                            ("FIXED"      :foreground "green"   :weight bold)))
  (org-use-fast-todo-selection 'expert)
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)
  (org-priority-faces '((?A :foreground "red")
                        (?B :foreground "orange")
                        (?C :foreground "yellow")))
  (org-global-properties '(("EFFORT_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00")
                           ("APPT_WARNTIME_ALL" . "0 5 10 15 20 25 30 45 60")
                           ("RISK_ALL" . "Low Medium High")
                           ("STYLE_ALL" . "habit")))
  (org-columns-default-format "%25ITEM %TODO %SCHEDULED %DEADLINE %3PRIORITY %TAGS %CLOCKSUM %EFFORT{:}")
  ;; Remove CLOSED: [timestamp] after switching to non-DONE states
  (org-closed-keep-when-no-todo t)
  ;; log
  (org-log-done 'time)
  (org-log-repeat 'time)
  (org-log-redeadline 'note)
  (org-log-reschedule 'note)
  (org-log-into-drawer t)
  (org-log-state-notes-insert-after-drawers nil)
  ;; refile
  (org-refile-use-cache t)
  (org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  ;; tags
  (org-tags-column 0)
  (org-use-tag-inheritance nil)
  (org-agenda-use-tag-inheritance nil)
  (org-use-fast-tag-selection t)
  (org-fast-tag-selection-single-key t)
  (org-track-ordered-property-with-tag t)
  (org-tag-persistent-alist '(("READ"  . ?r)
                              ("MAIL"  . ?@)
                              ("WRITE" . ?w)))
  (org-tag-alist '((:startgroup)
                   ("OWNER"    . ?o)
                   ("ASSIGNEE" . ?a)
                   ("OBSERVER" . ?b)
                   (:endgroup)))
  ;; archive
  (org-archive-location "%s_archive::datetree/"))

;; Keep track of tasks
(use-package org-agenda
  :ensure nil
  :after org
  :hook (org-agenda-finalize . org-agenda-to-appt)
  :config
  ;; update appt list every 5 minutes
  (run-at-time t 300 #'org-agenda-to-appt)
  (advice-add #'org-agenda-to-appt :around #'+suppress-message)
  :custom
  (org-agenda-files (list (expand-file-name "tasks.org" org-directory)))
  (org-agenda-diary-file (expand-file-name "diary.org" org-directory))
  (org-agenda-insert-diary-extract-time t)
  (org-agenda-compact-blocks t)
  (org-agenda-block-separator nil)
  (org-agenda-sticky t)
  ;; holidays
  (org-agenda-include-diary t)
  (org-agenda-include-deadlines t)
  (org-agenda-follow-indirect t)
  (org-agenda-inhibit-startup t)
  (org-agenda-show-all-dates t)
  (org-agenda-time-leading-zero t)
  (org-agenda-start-with-log-mode t)
  (org-agenda-start-with-clockreport-mode t)
  (org-agenda-remove-tags t)
  (org-agenda-todo-ignore-with-date nil)
  (org-agenda-todo-ignore-deadlines 'far)
  (org-agenda-todo-ignore-scheduled 'future)
  (org-agenda-todo-ignore-timestamp nil)
  (org-agenda-tags-todo-honor-ignore-options t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-timestamp-if-done t)
  (org-agenda-skip-unavailable-files t)
  (org-agenda-skip-scheduled-delay-if-deadline t)
  (org-agenda-skip-scheduled-if-deadline-is-shown t)
  (org-agenda-skip-additional-timestamps-same-entry t)
  (org-agenda-text-search-extra-files '(agenda-archives))
  (org-agenda-clockreport-parameter-plist
   '(:link t :maxlevel 5 :fileskip0 t :compact nil :narrow 80))
  (org-agenda-columns-add-appointments-to-effort-sum t)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-window-setup 'current-window)
  ;; starts from Monday
  (org-agenda-start-on-weekday 1)
  (org-agenda-use-time-grid t)
  (org-agenda-timegrid-use-ampm nil)
  (org-agenda-search-headline-for-time nil))

;; Write codes in org-mode
(use-package org-src
  :ensure nil
  :after org
  :hook (org-babel-after-execute . org-redisplay-inline-images)
  :bind (:map org-src-mode-map
         ;; consistent with separedit/magit
         ("C-c C-c" . org-edit-src-exit))
  :custom
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-src-preserve-indentation t)
  (org-src-window-setup 'current-window)
  (org-confirm-babel-evaluate nil)
  (org-edit-src-content-indentation 0)
  (org-src-lang-modes '(("C"      . c)
                        ("C++"    . c++)
                        ("bash"   . sh)
                        ("cpp"    . c++)
                        ("dot"    . graphviz-dot)
                        ("elisp"  . emacs-lisp)
                        ("ocaml"  . tuareg)
                        ("shell"  . sh)
                        ("sqlite" . sql)))
  (org-babel-load-languages '((awk        . t)
                              (C          . t)
                              (calc       . t)
                              (dot        . t)
                              (emacs-lisp . t)
                              (eshell     . t)
                              (ocaml      . t)
                              (python     . t)
                              (shell      . t)
                              (sql        . t))))

(use-package org-id
  :ensure nil
  :after org
  :custom
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))

(use-package org-goto
  :ensure nil
  :after org
  :custom
  (org-goto-auto-isearch nil)
  (org-goto-interface 'outline-path-completion))

;; Create structured information quickly
(use-package org-capture
  :ensure nil
  :after org doct
  :hook (org-capture-mode . (lambda ()
                              (setq-local org-complete-tags-always-offer-all-agenda-tags t)))
  :config
  ;; These variables/functions are used when capturing a minutes of meeting.
  (defvar org-capture--id-copy nil)

  (defun org-id-new-and-save ()
    "Get a new org-id via `org-id-new' then save it."
    (let ((id (org-id-new)))
      (setq org-capture--id-copy id)
      id))

  (defun org-id-load-from-copy ()
    "Read previously allocated org-id from local copy."
    org-capture--id-copy)
  :custom
  ;; `doct' requires that
  (org-capture-templates-contexts nil)
  (org-capture-use-agenda-date t)
  (org-capture-templates
   (doct `(:group
           :empty-lines 1
           :children
           (("Tasks"
             :keys "t"
             :file "tasks.org"
             :children
             (("Inbox"
               :keys "i"
               :type entry
               :prepend t
               :headline "Inbox"
               :template "* %?\n%i\n")
              ("Mail"
               :keys "m"
               :type entry
               :headline "Inbox"
               :template "* TODO %^{type|reply to|contact} %^{recipient} about %^{subject} :MAIL:\n")
              ("Reminder"
               :keys "r"
               :type entry
               :headline "Reminders"
               :template "* TODO %i%?")))
            ("Capture"
             :keys "c"
             :file "capture.org"
             :children
             (("Bookmark"
               :keys "b"
               :type entry
               :headline "Bookmarks"
               :immediate-finish t
               :template "* [[%:link][%:description]] :READ:\n %a\n %i")
              ("Note"
               :keys "n"
               :type entry
               :headline "Notes"
               :template "* %? %^g\n%i\n")
              ("Meeting"
               :keys "m"
               :type entry
               :olp ("Meeting")
               :datetree t
               :jump-to-captured t
               :template ,(concat "* %^{Subject} :MEETING:\n"
                                  ":PROPERTIES:\n"
                                  ":ID:         %(org-id-new-and-save)\n"
                                  ":CREATED:    %<%FT%T%z>\n"
                                  ":END:\n"
                                  "** Present at meeting\n"
                                  "- [ ] %^{Attendees}\n"
                                  "** Subjects\n"
                                  "- Comments and corrections to last meeting notes (delete me)\n"
                                  "- Reports from the sub teams (delete me)\n"
                                  "- Discussion (delete me)\n"
                                  "** Conclusion\n%?\n"
                                  "** Track\n"
                                  "#+BEGIN: columnview :id %(org-id-load-from-copy) :match \"/TODO|DONE\" :format \"\\%ITEM(What) \\%TAGS(Who) \\%RISK(Risk Level) \\%DEADLINE(Due) \\%TODO(State)\"\n#+END:\n"
                                  ))))))
         ))
  )

;; org links
(use-package ol
  :ensure nil
  :after org
  :custom
  (org-link-abbrev-alist '(("GitHub"        . "https://github.com/")
                           ("GitLab"        . "https://gitlab.com/")
                           ("Google"        . "https://google.com/search?q=")
                           ("RFCs"          . "https://tools.ietf.org/html/")
                           ("LWN"           . "https://lwn.net/Articles/")
                           ("StackOverflow" . "https://stackoverflow.com/q/%s")
                           ("WG21"          . "https://wg21.link/")
                           ("Wikipedia"     . "https://en.wikipedia.org/wiki/")
                           ("YouTube"       . "https://youtube.com/watch?v=")
                           ("Zhihu"         . "https://zhihu.com/question/"))))

;; Declarative Org Capture Templates
(use-package doct
  :ensure t
  :commands doct doct-get
  :demand t)

(provide 'init-org)

;;; init-org.el ends here
