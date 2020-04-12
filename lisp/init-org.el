;;; init-org.el --- Org mode configurations -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package org
  :ensure nil
  :mode ("\\.org\\'" . org-mode)
  :hook (org-mode . auto-fill-mode)
  :custom
  (org-tags-column -80)
  (org-catch-invisible-edits 'smart)
  (org-insert-heading-respect-content t)
  ;; block switching the parent to done state
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)
  ;; nice look
  (org-ellipsis " ▼ ")
  (org-list-demote-modify-bullet '(("+" . "-") ("1." . "a.") ("-" . "+")))
  (org-todo-keywords '((sequence "TODO(t)" "INPROGRESS(i!)" "WAITING(w!)"
                                 "|" "DONE(d!)" "CANCELLED(c!)")))
  (org-todo-keyword-faces
   '(("TODO"       :foreground "#7c7c75" :weight normal :underline t)
     ("INPROGRESS" :foreground "#0098dd" :weight normal :underline t)
     ("WAITING"    :foreground "#9f7efe" :weight normal :underline t)
     ("DONE"       :foreground "#50a14f" :weight normal :underline t)
     ("CANCELLED"  :foreground "#ff6480" :weight normal :underline t)))
  ;; log
  (org-log-done 'time)
  (org-log-repeat 'time)
  (org-log-redeadline 'note)
  (org-log-reschedule 'note)
  (org-log-into-drawer t)
  (org-log-state-notes-insert-after-drawers nil))

;; Keep track of tasks
(use-package org-agenda
  :ensure nil
  :after org
  :custom
  (org-agenda-files '("~/.org/"))
  (org-agenda-diary-file "~/.org/diary.org")
  (org-agenda-include-deadlines t)
  (org-agenda-inhibit-startup t)
  (org-agenda-show-all-dates t)
  (org-agenda-start-with-log-mode t)
  (org-agenda-start-with-clockreport-mode t)
  (org-agenda-hide-tags-regexp ":\\w+:")
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-timestamp-if-done t)
  (org-agenda-skip-unavailable-files t)
  (org-agenda-window-setup 'current-window)
  ;; starts from Monday
  (org-agenda-start-on-weekday 1)
  (org-agenda-use-time-grid t)
  (org-agenda-timegrid-use-ampm nil)
  (org-agenda-time-grid '((daily tody require-timed)
                          (300 600 900 1200 1500 1800 2100 2400)
                          "......" "----------------"))
  (org-agenda-search-headline-for-time nil))

;; Record the time
(use-package org-clock
  :ensure nil
  :after org
  :custom
  (org-clock-in-resume t)
  (org-clock-persist 'history)
  (org-clock-out-remove-zero-time-clocks t)
  :config
  (org-clock-persistence-insinuate))

;; Write codes in org-mode
(use-package org-src
  :ensure nil
  :after org
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
  (org-babel-load-languages '((shell . t)
                              (python . t)
                              (ocaml . t)
                              (emacs-lisp . t))))

;; Create structured information quickly
(use-package org-capture
  :ensure nil
  :after org
  :custom
  (org-capture-templates
   '(("a" "Append")
     ("c" "Captures")
     ("ct" "Task" entry (file+headline "~/.org/tasks.org" "INBOX")
      "* TODO %^{taskname} %^{CATEGORY}p\n :PROPERTIES:\n :CREATED: %U\n :END:\n")
     ("cr" "Reference" entry (file+headline "~/.org/reference.org")
      "* TODO %u %^{reference}\n %?")))
  )

;; export
(use-package ox
  :ensure nil
  :custom
  (org-export-with-toc t)
  (org-export-with-email t)
  (org-export-with-author t)
  (org-export-with-drawers nil)
  (org-export-with-footnotes t)
  (org-export-with-smart-quotes t)
  (org-export-with-section-numbers t)
  (org-export-with-sub-superscripts nil)
  (org-export-in-background t)
  (org-export-headline-levels 5)
  (org-export-backends '(ascii html md)))

;; Pretty symbols
(use-package org-superstar
  :ensure t
  :custom
  ;; hide leading stars
  (org-superstar-leading-bullet ?\s)
  ;; fancy todo items
  (org-superstar-special-todo-items t)
  :hook (org-mode . org-superstar-mode))

;; Declarative Org Capture Templates
(use-package doct
  :ensure t
  :after org
  :commands (doct))

(provide 'init-org)

;;; init-org.el ends here
