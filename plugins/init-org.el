;;; init-org.el --- Org mode configurations -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package org
  :ensure nil
  :mode ("\\.org\\'" . org-mode)
  :hook (org-mode . auto-fill-mode)
  :bind (:map org-src-mode-map
         ;; consistent with separedit/magit
         ("C-c C-c" . org-edit-src-exit))
  :custom
  ;; agenda
  (org-agenda-inhibit-startup t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-unavailable-files t)
  (org-agenda-window-setup 'current-window)
  ;; todo
  (org-tags-column -80)
  (org-todo-keywords
   '((sequence "TODO(t)" "INPROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (org-todo-keyword-faces
   '(("TODO"       :foreground "#7c7c75" :weight normal :underline t)
     ("INPROGRESS" :foreground "#0098dd" :weight normal :underline t)
     ("WAITING"    :foreground "#9f7efe" :weight normal :underline t)
     ("DONE"       :foreground "#50a14f" :weight normal :underline t)
     ("CANCELLED"  :foreground "#ff6480" :weight normal :underline t)))
  (org-log-done 'time)
  ;; babel
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-src-preserve-indentation t)
  (org-confirm-babel-evaluate nil)
  (org-babel-load-languages '((shell . t)
                              (python . t)
                              (plantuml . t)
                              (ocaml . t)
                              (emacs-lisp . t))))

;; Pretty symbols
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

(provide 'init-org)

;;; init-org.el ends here
