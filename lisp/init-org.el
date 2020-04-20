;;; init-org.el --- Org mode configurations -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package org
  :ensure nil
  :mode ("\\.org\\'" . org-mode)
  :hook (org-mode . auto-fill-mode)
  :commands (org-find-exact-headline-in-buffer org-set-tags)
  :custom
  (org-directory "~/.org")
  (org-tags-column 0)
  (org-pretty-entities t)
  (org-startup-indented t)
  (org-image-actual-width nil)
  (org-hide-emphasis-markers t)
  (org-fontify-done-headline t)
  (org-fontify-whole-heading-line t)
  (org-fontify-quote-and-verse-blocks t)
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
   '(("TODO"       :foreground "#7c7c75" :weight bold)
     ("INPROGRESS" :foreground "#0098dd" :weight bold)
     ("WAITING"    :foreground "#9f7efe" :weight bold)
     ("DONE"       :foreground "#50a14f" :weight bold)
     ("CANCELLED"  :foreground "#ff6480" :weight bold)))
  (org-highest-priority ?A)
  (org-lowest-priority ?E)
  (org-default-priority ?C)
  (org-priority-faces '((?A :foreground "red")
                        (?B :foreground "orange")
                        (?C :foreground "yellow")
                        (?D :foreground "green")
                        (?E :foreground "blue")))
  ;; log
  (org-log-done 'time)
  (org-log-repeat 'time)
  (org-log-redeadline 'note)
  (org-log-reschedule 'note)
  (org-log-into-drawer t)
  (org-log-state-notes-insert-after-drawers nil)
  ;; archive
  (org-archive-location "%s_archive::date-tree"))

;; Keep track of tasks
(use-package org-agenda
  :ensure nil
  :after org
  :custom
  (org-agenda-files `(,org-directory))
  (org-agenda-diary-file "diary.org")
  (org-agenda-sticky t)
  (org-agenda-span 10)
  (org-agenda-include-diary nil)
  (org-agenda-include-deadlines t)
  (org-agenda-inhibit-startup t)
  (org-agenda-show-all-dates t)
  (org-agenda-time-leading-zero t)
  (org-agenda-start-with-log-mode t)
  (org-agenda-start-with-clockreport-mode t)
  (org-agenda-hide-tags-regexp ":\\w+:")
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-timestamp-if-done t)
  (org-agenda-skip-unavailable-files t)
  (org-agenda-columns-add-appointments-to-effort-sum t)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-window-setup 'current-window)
  ;; starts from Monday
  (org-agenda-start-on-weekday 1)
  (org-agenda-start-day "-3d")
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
  (org-clock-idle-time 10)
  (org-clock-out-when-done t)
  (org-clock-persist 'history)
  (org-clock-history-length 10)
  (org-clock-out-remove-zero-time-clocks t)
  (org-clock-report-include-clocking-task t)
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
  :after org doct
  :custom
  (org-capture-use-agenda-date t)
  ;; https://www.reddit.com/r/emacs/comments/fs7tk3/how_to_manage_todo_tasks_in_my_project/
  (org-capture-templates
   (doct `(:group
            :empty-lines 1
            :children
            (("Tasks"
              :keys "t"
              :file "tasks.org"
              :clock-in t
              :clock-resume t
              :children
              (("Today" :keys "t" :type entry :headline "Uncategorized"
                :datetree t :tree-type week :template "* TODO %?\n %i\n %a\n")
               ("Reading" :keys "r" :type entry :headline "Reading"
                :template "* TODO %^{name}\n %a\n")
               ("Work" :keys "w" :type entry :headline "Work"
                :template "* TODO %^{taskname}\n %a\n")))
             ("Project"
              :keys "p"
              :file ,(defun my/project-todo-file ()
                       (let ((file (expand-file-name "TODO.org"
                                                     (when (functionp 'projectile-project-root)
                                                       (projectile-project-root)))))
                         (with-current-buffer (find-file-noselect file)
                           (org-mode)
                           ;; Set to UTF-8 because we may be visiting raw file
                           (setq buffer-file-coding-system 'utf-8-unix)
                           (when-let* ((headline (doct-get :headline)))
                             (unless (org-find-exact-headline-in-buffer headline)
                               (goto-char (point-max))
                               (insert "* " headline)
                               (org-set-tags (downcase headline))))
                           file)))
              :template (lambda () (concat "* %{todo-state} " (when (y-or-n-p "Link? ") "%A\n") "%?"))
              :todo-state "TODO"
              :children (("bug"           :keys "b" :headline "Bugs")
                         ("documentation" :keys "d" :headline "Documentation")
                         ("enhancement"   :keys "e" :headline "Enhancements")
                         ("feature"       :keys "f" :headline "Features")
                         ("optimization"  :keys "o" :headline "Optimizations")
                         ("miscellaneous" :keys "m" :headline "Miscellaneous")
                         ("security"      :keys "s" :headline "Security")))))
         ))
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

(use-package ox-html
  :ensure nil
  :custom
  (org-html-validation-link nil))

;; Pretty symbols
(use-package org-superstar
  :ensure t
  :custom
  ;; hide leading stars, rendering in spaces
  (org-hide-leading-stars nil)
  (org-superstar-leading-bullet ?\s)
  :hook (org-mode . org-superstar-mode))

;; Declarative Org Capture Templates
(use-package doct
  :ensure t
  :demand t
  :commands (doct doct-get))

(provide 'init-org)

;;; init-org.el ends here
