;;; init-selectrum.el --- Incremental completion system and other tools -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; Easily select item from list
;;
;; faster than the builtin `icomplete-mode'
(use-package selectrum
  :ensure t
  :hook (after-init . selectrum-mode)
  :custom
  (selectrum-fix-vertical-window-height t))

;; Writable grep buffer
(use-package wgrep
  :ensure t
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

;; Emacs Mini-Buffer Actions Rooted in Keymaps
;;
;; `embark-export' is more like `ivy-occur'.
(use-package embark
  :ensure t
  :bind (:map minibuffer-local-map
         ("M-o"     . embark-act)
         ("C-c C-o" . embark-export)
         ("C-c C-c" . embark-collect-snapshot)))

;; Consulting `completing-read'
(use-package consult
  :ensure t
  :bind (([remap apropos]            . consult-apropos)
         ([remap bookmark-jump]      . consult-bookmark)
         ([remap imenu]              . consult-imenu)
         ([remap recentf-open-files] . consult-recent-file)
         ([remap yank-pop]           . consult-yank-pop))
  :custom
  ;; Disable preview
  (consult-preview-key nil))

;; Make `embark-export' to `occur-mode' or `grep-mode'
;;
;; `consult-grep' -> `embark-export' -> `grep-mode'
;; `consult-line' -> `embark-export' -> `occur-mode'
(use-package embark-consult
  :ensure t
  :after embark consult)

(provide 'init-selectrum)
;;; init-selectrum.el ends here
