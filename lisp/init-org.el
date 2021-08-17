;;; init-org.el --- Org mode configurations -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'init-macros)

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
    (cl-letf (((symbol-function 'org-fit-window-to-buffer)
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
  (org-modules '(ol-eww org-id ol-info ol-eshell))
  ;; prettify
  (org-fontify-todo-headline t)
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-heading-line t)
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
  (org-log-repeat 'time)
  (org-log-into-drawer t)
  ;; refile
  (org-refile-use-cache t)
  (org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  ;; goto
  (org-goto-auto-isearch nil)
  (org-goto-interface 'outline-path-completion)
  ;; tags
  (org-use-tag-inheritance nil)
  (org-agenda-use-tag-inheritance nil)
  (org-use-fast-tag-selection t)
  (org-fast-tag-selection-single-key t)
  (org-track-ordered-property-with-tag t)
  ;; archive
  (org-archive-location "%s_archive::datetree/")
  ;; id
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))

;; Keep track of tasks
(use-package org-agenda
  :ensure nil
  :after org
  :hook (org-agenda-finalize . org-agenda-to-appt)
  :config
  ;; update appt list every 5 minutes
  (run-at-time t 300 #'org-agenda-to-appt)
  (shut-up! #'org-agenda-to-appt)
  :custom
  (org-agenda-files (list (expand-file-name "tasks.org" org-directory)))
  (org-agenda-diary-file (expand-file-name "diary.org" org-directory))
  (org-agenda-insert-diary-extract-time t)
  (org-agenda-inhibit-startup t)
  (org-agenda-time-leading-zero t)
  (org-agenda-remove-tags t)
  (org-agenda-columns-add-appointments-to-effort-sum t)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-window-setup 'current-window))

;; Write codes in org-mode
(use-package org-src
  :ensure nil
  :after org
  :hook (org-babel-after-execute . org-redisplay-inline-images)
  :bind (:map org-src-mode-map
         ;; consistent with separedit/magit
         ("C-c C-c" . org-edit-src-exit))
  :custom
  (org-confirm-babel-evaluate nil)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-src-preserve-indentation t)
  (org-src-window-setup 'other-window)
  (org-src-lang-modes '(("C"      . c)
                        ("C++"    . c++)
                        ("bash"   . sh)
                        ("cpp"    . c++)
                        ("dot"    . graphviz-dot) ;; was `fundamental-mode'
                        ("elisp"  . emacs-lisp)
                        ("ocaml"  . tuareg)
                        ("shell"  . sh)))
  (org-babel-load-languages '((C          . t)
                              (dot        . t)
                              (emacs-lisp . t)
                              (eshell     . t)
                              (python     . t)
                              (shell      . t))))

;; Create structured information quickly
(use-package org-capture
  :ensure nil
  :after org
  :hook (org-capture-mode . (lambda ()
                              (setq-local org-complete-tags-always-offer-all-agenda-tags t)))
  :custom
  (org-capture-use-agenda-date t)
  (org-capture-templates-contexts nil)
  (org-capture-templates '(("t" "Tasks")
                           ("ti" "Inbox" entry (file+headline "tasks.org" "Inbox")
                            "* %?\n%i\n")
                           ("tm" "Mail" entry (file+headline "tasks.org" "Inbox")
                            "* TODO %^{type|reply to|contact} %^{recipient} about %^{subject} :MAIL:\n")
                           ("c" "Capture")
                           ("cn" "Note" entry (file+headline "capture.org" "Notes")
                            "* %? %^g\n%i\n"))))

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

(provide 'init-org)
;;; init-org.el ends here
