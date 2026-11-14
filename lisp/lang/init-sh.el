;;; init-sh.el --- Shell scripts -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; Edit shell scripts
;;
;; sh-mode provides `sh-while-getopts' to automate getopts.
(use-package sh-script
  :ensure nil
  :hook (sh-mode . sh-mode-setup)
  :config
  (defun sh-mode-setup ()
    (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p nil t))
  :custom
  (sh-basic-offset 2))

;; Snippets for sh
(use-package tempo
  :ensure nil
  :hook (sh-mode . sh-mode-tempo-setup)
  :config
  (defvar sh-tempo-tags nil)
  (defun sh-mode-tempo-setup ()
    (tempo-use-tag-list 'sh-tempo-tags))

  (tempo-define-template "sh-shebang"
                         '("#!/bin/bash" > n n
                           "set -euo pipefail -x" > n n
                           (when (and (derived-mode-p 'sh-base-mode)
                                      (not (eq (bound-and-true-p sh-shell) 'bash)))
                             (sh-set-shell "bash" nil nil)
                             nil))
                         "shebang"
                         "Insert shebang"
                         'sh-tempo-tags))

(provide 'init-sh)
;;; init-sh.el ends here
