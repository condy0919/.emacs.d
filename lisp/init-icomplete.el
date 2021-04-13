;;; init-icomplete.el --- Incremental completion system and other tools -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; The builtin incremental completion system
(use-package icomplete
  :ensure nil
  :hook (after-init . icomplete-mode)
  :custom
  (icomplete-vertical-mode t)
  (icomplete-prospects-height 10)
  (icomplete-hide-common-prefix nil)
  (icomplete-show-matches-on-no-input t)
  :custom-face
  ;; More obvious to distinguish
  (icomplete-first-match ((t (:inherit highlight)))))

;; Writable grep buffer
(use-package wgrep
  :ensure t
  :defer 1
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

;; Emacs Mini-Buffer Actions Rooted in Keymaps
(use-package embark
  :ensure t
  :bind (:map minibuffer-local-map
         ("M-o"     . embark-act)
         ("C-c C-o" . embark-collect-snapshot)
         ("C-c C-c" . embark-export)))

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
  (consult-preview-key nil)
  (consult-narrow-key "<"))

(provide 'init-icomplete)
;;; init-icomplete.el ends here
