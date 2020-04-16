;;; init-git.el --- Git is awesome -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; The awesome git client
(use-package magit
  :ensure t
  :defer 1
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ;; Close transient with ESC
         :map transient-map
         ([escape] . transient-quit-one))
  :custom
  ;; Supress message
  (magit-no-message '("Turning on magit-auto-revert-mode..."))
  (magit-diff-refine-hunk t)
  (magit-status-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18)))

;; Todo integration
(use-package magit-todos
  :ensure t
  :after magit
  :bind (:map magit-todos-section-map
         ("j" . nil)
         :map magit-todos-item-section-map
         ("j" . nil))
  :hook (magit-status-mode . magit-todos-mode)
  :config
  ;; Supress the `jT' keybind warning
  (define-advice magit-todos-mode (:around (func &rest args))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest args) nil)))
      (apply func args))))

;; Dont display empty groups
(use-package ibuffer
  :ensure nil
  :custom
  (ibuffer-movement-cycle nil)
  (ibuffer-show-empty-filter-groups nil))

;; Group buffers by git/svn/... project
(use-package ibuffer-vc
  :ensure t
  :commands (ibuffer-do-sort-by-alphabetic)
  :hook (ibuffer . (lambda ()
                     (ibuffer-vc-set-filter-groups-by-vc-root)
                     (unless (eq ibuffer-sorting-mode 'alphabetic)
                       (ibuffer-do-sort-by-alphabetic)))))

(use-package vc
  :ensure nil
  :custom
  (vc-follow-symlinks t)
  (vc-handled-backends nil))

;; Spell check
(use-package flyspell
  :ensure nil
  :if (executable-find "hunspell")
  :hook (git-commit-mode . flyspell-mode)
  :custom
  (ispell-dictionary "en_US")
  (ispell-program-name "hunspell")
  (ispell-personal-dictionary
   (expand-file-name "hunspell_dict.txt" user-emacs-directory))
  ;; "C-;" is captured by fcitx
  (flyspell-auto-correct-binding (kbd "C-M-;"))
  (flyspell-issue-welcome-flag nil)
  (flyspell-issue-message-flag nil))

;; Highlight uncommitted changes using git
(use-package diff-hl
  :ensure t
  :hook ((prog-mode . (lambda ()
                        (diff-hl-mode)
                        (diff-hl-flydiff-mode)))
         (magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode . diff-hl-dired-mode-unless-remote)))

;; Open current file in browser
(use-package browse-at-remote
  :ensure t
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

(provide 'init-git)

;;; init-git.el ends here
