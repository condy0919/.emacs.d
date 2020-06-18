;;; init-git.el --- Git is awesome -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package transient
  :ensure t
  :commands (transient-setup transient-prefix)
  :bind (:map transient-map
         ;; Close transient with ESC
         ([escape] . transient-quit-one)))

;; The awesome git client
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch))
  :custom
  ;; Supress message
  (magit-no-message '("Turning on magit-auto-revert-mode..."))
  (magit-process-popup-time 30)
  (magit-ediff-dwim-show-on-hunks t)
  (magit-diff-refine-hunk t))

;; Todo integration
(use-package magit-todos
  :ensure t
  :hook (magit-status-mode . magit-todos-mode))

;; NB `diff-hl' depends on `vc'
(use-package vc
  :ensure nil
  :custom
  ;; Disable vc for remote files, and `diff-hl' won't work as expected.
  (vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
                                vc-ignore-dir-regexp
                                tramp-file-name-regexp))
  (vc-follow-symlinks t)
  (vc-handled-backends '(Git)))

;; Highlight uncommitted changes using git
(use-package diff-hl
  :ensure t
  :hook ((after-init         . (lambda ()
                                 (global-diff-hl-mode)
                                 (diff-hl-flydiff-mode)))
         (magit-pre-refresh  . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode         . diff-hl-dired-mode-unless-remote)))

;; Open current file in browser
(use-package browse-at-remote
  :ensure t
  :custom
  (browse-at-remote-add-line-number-if-no-region-selected nil)
  :bind (:map vc-prefix-map
         ("b" . bar-browse)
         ("c" . bar-to-clipboard)))

;; Pop up last commit information of current line
(use-package git-messenger
  :ensure t
  :bind (:map vc-prefix-map
         ("p" . git-messenger:popup-message)
         :map git-messenger-map
         ("m" . git-messenger:copy-message))
  :custom
  (git-messenger:show-detail t)
  (git-messenger:use-magit-popup t))

;; Setup gitignore mode
(use-package conf-mode
  :ensure nil
  :mode (("\\.gitignore\\'" . conf-unix-mode)))

(use-package smerge-mode
  :ensure nil
  :requires transient
  :bind (:map smerge-mode-map
         ("C-c m" . my/smerge-menu))
  :config
  (transient-define-prefix my/smerge-menu
    "Smerge"
    [["Navigation"
      ("p" "prev" smerge-prev)
      ("n" "next" smerge-next)]
     ["Keep"
      ("b" "base"    smerge-keep-base)
      ("u" "upper"   smerge-keep-upper)
      ("l" "lower"   smerge-keep-lower)
      ("a" "all"     smerge-keep-all)
      ("c" "current" smerge-keep-current)]
     ["Diff"
      ("<" "base against upper"  smerge-diff-base-upper)
      ("=" "upper against lower" smerge-diff-upper-lower)
      (">" "base against lower"  smerge-diff-base-lower)
      ("R" "refine"              smerge-refine)
      ("E" "ediff"               smerge-ediff)]
     ["Other"
      ("C" "combine"   smerge-combine-with-next)
      ("r" "resolve"   smerge-resolve)
      ("k" "kill"      smerge-kill-current)
      ("h" "highlight" smerge-refine)]])
  )

(provide 'init-git)

;;; init-git.el ends here
