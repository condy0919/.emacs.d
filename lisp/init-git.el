;;; init-git.el --- Git is awesome -*- lexical-binding: t -*-

;;; Commentary:
;;
;; git-messenger has been superseded by {C-x v h} (`vc-region-history')

;;; Code:

;; The awesome git client
;;
;; Explicit binding makes it load lazily although it's the default.
;; See `magit-define-global-key-bindings' for more information.
(use-package magit
  :ensure t
  :hook (git-commit-mode . flyspell-mode)
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c M-g" . magit-file-dispatch))
  :custom
  ;; Supress message
  (magit-no-message '("Turning on magit-auto-revert-mode..."))
  (magit-save-repository-buffers 'dontask)
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
  :config
  ;; Disable vc for remote files, and `diff-hl' won't work as expected.
  (setq vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
                                     vc-ignore-dir-regexp
                                     tramp-file-name-regexp))
  :custom
  (vc-follow-symlinks t)
  (vc-handled-backends '(Git)))

;; Highlight uncommitted changes using VC
(use-package diff-hl
  :ensure t
  :hook ((after-init         . (lambda ()
                                 (global-diff-hl-mode)
                                 (diff-hl-flydiff-mode)))
         (magit-pre-refresh  . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode         . diff-hl-dired-mode-unless-remote)))

;; Visual diff interface
(use-package ediff
  :ensure nil
  :hook (ediff-quit . winner-undo) ;; restore windows layout
  :custom
  (ediff-diff-options "-w") ;; turn off whitespace checking
  (ediff-highlight-all-diffs t)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-merge-split-window-function 'split-window-horizontally))

;; A minor mode to resolve conflict
(use-package smerge-mode
  :ensure nil
  :config
  (with-eval-after-load 'hydra
    (defhydra hydra-smerge (:color pink
                            :hint nil
                            :pre (unless smerge-mode (smerge-mode +1))
                            :post (smerge-auto-leave))
      "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
      ("n" smerge-next)
      ("p" smerge-prev)
      ("b" smerge-keep-base)
      ("u" smerge-keep-upper)
      ("l" smerge-keep-lower)
      ("a" smerge-keep-all)
      ("RET" smerge-keep-current)
      ("\C-m" smerge-keep-current)
      ("<" smerge-diff-base-upper)
      ("=" smerge-diff-upper-lower)
      (">" smerge-diff-base-lower)
      ("R" smerge-refine)
      ("E" smerge-ediff)
      ("C" smerge-combine-with-next)
      ("r" smerge-resolve)
      ("k" smerge-kill-current)
      ("q" nil "cancel" :color blue)))
  )

;; Open current file in browser
(use-package browse-at-remote
  :ensure t
  :bind (:map vc-prefix-map
         ("b" . bar-browse)         ;; was `vc-switch-backend'
         ("c" . bar-to-clipboard))
  :custom
  (browse-at-remote-add-line-number-if-no-region-selected nil))

;; Setup gitignore mode
(use-package conf-mode
  :ensure nil
  :mode (("\\.gitignore\\'" . conf-unix-mode)))

(provide 'init-git)

;;; init-git.el ends here
