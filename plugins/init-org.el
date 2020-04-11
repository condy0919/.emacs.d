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
  (org-todo-keywords
   '((sequence "TODO(t)" "INPROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (org-todo-keyword-faces
   '(("TODO"       :foreground "#7c7c75" :weight normal :underline t)
     ("INPROGRESS" :foreground "#0098dd" :weight normal :underline t)
     ("WAITING"    :foreground "#9f7efe" :weight normal :underline t)
     ("DONE"       :foreground "#50a14f" :weight normal :underline t)
     ("CANCELLED"  :foreground "#ff6480" :weight normal :underline t)))
  ;; log
  (org-log-done 'time)
  )

;; Keep track of tasks
(use-package org-agenda
  :ensure nil
  :after org
  :custom
  (org-agenda-files '("~/.org/"))
  (org-agenda-diary-file "~/.org/diary.org")
  (org-agenda-inhibit-startup t)
  (org-agenda-show-all-dates t)
  (org-agenda-hide-tags-regexp ":\\w+:")
  (org-enforce-todo-checkbox-dependencies t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-unavailable-files t)
  (org-agenda-window-setup 'current-window)
  ;; consistent with builtin calendar
  (org-agenda-start-on-weekday 1)
  (org-agenda-search-headline-for-time nil))

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
  (org-edit-src-content-indentation 0)
  (org-src-window-setup 'current-window)
  (org-confirm-babel-evaluate nil)
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

;; Pretty symbols
(use-package org-superstar
  :ensure t
  :custom
  ;; hide leading stars
  (org-superstar-leading-bullet ?\s)
  ;; fancy todo items
  (org-superstar-special-todo-items t)
  :hook (org-mode . org-superstar-mode))

(provide 'init-org)

;;; init-org.el ends here
