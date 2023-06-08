;;; init-sh.el --- Shell scripts -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; Edit shell scripts
;;
;; sh-mode provides `sh-while-getopts' to automate getopts.
(use-package sh-script
  :ensure nil
  :mode (("\\.sh\\'"     . sh-mode)
         ("/PKGBUILD\\'" . sh-mode))
  :hook (sh-mode . sh-mode-setup)
  :bind (:map sh-mode-map
         ("C-c C-e" . sh-execute-region))
  :config
  (defun sh-mode-setup ()
    (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p nil t))
  :custom
  (sh-basic-offset 2)
  (sh-indentation 2))

;; Snippets for sh
(use-package tempo
  :ensure nil
  :after sh-script
  :hook (sh-mode . sh-mode-tempo-setup)
  :config
  (defvar sh-tempo-tags nil)
  (defun sh-mode-tempo-setup ()
    (tempo-use-tag-list 'sh-tempo-tags))

  (tempo-define-template "sh-shebang"
                         '("#!/bin/bash" > n n
                           "set -euo pipefail -x" > n n
                           )
                         "shebang"
                         "Insert shebang"
                         'sh-tempo-tags))

(provide 'init-sh)
;;; init-sh.el ends here
