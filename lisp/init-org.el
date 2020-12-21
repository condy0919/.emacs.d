;;; init-org.el --- Org mode configurations -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(eval-when-compile
  (require 'init-core))

(use-package org
  :ensure nil
  :mode ("\\.org\\'" . org-mode)
  :hook (org-mode . visual-line-mode)
  :commands (org-find-exact-headline-in-buffer org-set-tags)
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
  (org-pretty-entities t)
  (org-startup-indented t)
  (org-startup-with-inline-images t)
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
  (advice-add #'org-agenda-to-appt :around #'my/suppress-message)
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

;; Record the time
(use-package org-clock
  :ensure nil
  :after org
  :functions notify-send
  :config
  (org-clock-persistence-insinuate)
  :custom
  (org-clock-in-resume t)
  (org-clock-idle-time 15)
  (org-clock-into-drawer t)
  (org-clock-out-when-done t)
  (org-clock-persist 'history)
  (org-clock-history-length 20)
  (org-clock-mode-line-total 'today)
  (org-clock-display-default-range 'thisweek)
  (org-clock-in-switch-to-state "WIP")
  (org-clock-out-switch-to-state "WAIT")
  (org-clock-out-remove-zero-time-clocks t)
  (org-clock-report-include-clocking-task t)
  (org-show-notification-handler (lambda (msg)
                                   (notify-send :title "Org Clock"
                                                :body msg
                                                :timeout 5000
                                                :urgency 'critical))))

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
                              (gnuplot    . t)
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

(use-package org-table
  :ensure nil
  :after org
  :custom
  (org-table-header-line-p t)
  (org-table-export-default-format "orgtbl-to-csv")
  (org-table-formula-constants '(("PI" . "3.14159265358979323846264"))))

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
                                  "** Agenda\n"
                                  "- Comments and corrections to last meeting notes (delete me)\n"
                                  "- Reports from the sub teams (delete me)\n"
                                  "- Discussion (delete me)\n"
                                  "** Notes\n%?\n"
                                  "** Actions\n"
                                  "#+BEGIN: columnview :id %(org-id-load-from-copy) :match \"/TODO|DONE\" :format \"\\%ITEM(What) \\%TAGS(Who) \\%RISK(Risk Level) \\%DEADLINE(When) \\%TODO(State)\"\n#+END:\n"
                                  "** Decisions\n"
                                  "#+BEGIN: columnview :id %(org-id-load-from-copy) :match \"DECISION\" :format \"\\%ITEM(Decision)\"\n#+END:\n"
                                  ))))))
         ))
  )

;; org links
(use-package ol
  :ensure nil
  :after org
  :custom
  (org-link-keep-stored-after-insertion t)
  (org-link-abbrev-alist '(("Arxiv"         . "https://arxiv.org/abs/")
                           ("GitHub"        . "https://github.com/")
                           ("GitLab"        . "https://gitlab.com/")
                           ("Google"        . "https://google.com/search?q=")
                           ("RFCs"          . "https://tools.ietf.org/html/")
                           ("LWN"           . "https://lwn.net/Articles/")
                           ("StackOverflow" . "https://stackoverflow.com/questions/%s/")
                           ("WG21"          . "https://wg21.link/")
                           ("Wikipedia"     . "https://en.wikipedia.org/wiki/")
                           ("YouTube"       . "https://youtube.com/watch?v=")
                           ("Zhihu"         . "https://zhihu.com/question/"))))

;; export
(use-package ox
  :ensure nil
  :after org
  :custom
  (org-export-with-toc t)
  (org-export-with-tags 'not-in-toc)
  (org-export-with-email nil)
  (org-export-with-author nil)
  (org-export-with-drawers nil)
  (org-export-with-priority t)
  (org-export-with-footnotes t)
  (org-export-with-smart-quotes t)
  (org-export-with-section-numbers nil)
  (org-export-with-sub-superscripts '{})
  ;; Use :eval never-export header argument to avoid evaluating.
  (org-export-use-babel t)
  (org-export-headline-levels 5)
  (org-export-coding-system 'utf-8)
  (org-export-with-broken-links 'mark)
  (org-export-backends '(ascii html md icalendar man)))

(use-package ox-ascii
  :ensure nil
  :after org
  :custom
  (org-ascii-charset 'utf-8))

(use-package ox-html
  :ensure nil
  :after org
  :custom
  (org-html-doctype "html5")
  (org-html-html5-fancy t)
  (org-html-checkbox-type 'unicode)
  (org-html-validation-link nil))

(use-package htmlize
  :ensure t
  :defer t
  :custom
  (htmlize-pre-style t)
  (htmlize-output-type 'inline-css))

(use-package ox-md
  :ensure nil
  :after org
  :custom
  (org-md-headline-style 'atx))

(use-package ox-icalendar
  :ensure nil
  :after org
  :custom
  (org-icalendar-include-todo 'all)
  (org-icalendar-use-scheduled '(todo-start event-if-todo event-if-not-todo))
  (org-icalendar-use-deadline '(todo-due event-if-todo event-if-not-todo))
  (org-icalendar-store-UID t))

(use-package ox-man
  :ensure nil
  :after org
  :custom
  (org-man-source-highlight t))

;; Pretty symbols
(use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode)
  :custom
  ;; hide leading stars, rendering in spaces
  (org-hide-leading-stars nil)
  (org-superstar-leading-bullet ?\s))

;; Super agenda mode
(use-package org-super-agenda
  :ensure t
  :hook (org-agenda-mode . org-super-agenda-mode)
  :custom
  (org-super-agenda-groups '((:order-multi (1 (:name "Done Today"
                                               :log closed)
                                              (:name "Clocked Today"
                                               :log clocked)))
                             (:name "Schedule" :time-grid t)
                             (:name "Today" :scheduled today)
                             (:habit t)
                             (:name "Due Today" :deadline today :face warning)
                             (:name "Overdue" :deadline past :face error)
                             (:name "Due Soon" :deadline future)
                             (:name "Scheduled Earlier" :scheduled past))))

;; Declarative Org Capture Templates
(use-package doct
  :ensure t
  :commands doct doct-get
  :demand t)

(use-package org-edna
  :ensure t
  :hook (org-mode . org-edna-mode)
  :custom
  (org-edna-finder-use-cache t)
  (org-edna-timestamp-format 'long))

;; Ensure that emacsclient.desktop exists and server-mode is opened.
;;
;; cat > ~/.local/share/applications/emacsclient.desktop << EOF
;; [Desktop Entry]
;; Name=Emacs Client
;; Exec=emacsclient %u
;; Icon=emacs-icon
;; Type=Application
;; Terminal=false
;; MimeType=x-scheme-handler/org-protocol;
;; EOF
(use-package org-protocol
  :ensure nil
  :after org
  :custom
  (org-protocol-default-template-key "cb"))

(use-package org-habit
  :ensure nil
  :after org
  :custom
  (org-habit-show-habits t)
  (org-habit-show-all-today t))

;; Dynamic headlines numbering
(use-package org-num
  :ensure nil
  :commands org-num-mode
  :after org
  :custom
  (org-num-skip-commented t)
  (org-num-skip-footnotes t)
  (org-num-skip-unnumbered t)
  (org-num-skip-tags `(,org-archive-tag)))

(provide 'init-org)

;;; init-org.el ends here
