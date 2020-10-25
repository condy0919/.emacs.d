;;; init-sh.el --- Shell scripts -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; Edit shell scripts
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

;; Snippets for shell scripts
(use-package tempo
  :ensure nil
  :after sh-script
  :hook (sh-mode . sh-mode-tempo-setup)
  :config
  (defvar sh-tempo-tags nil)

  (defun sh-mode-tempo-setup ()
    (tempo-use-tag-list 'sh-tempo-tags))

  (tempo-define-template "sh-getopt"
                         '("while getopts " p " opt; do" > n>
                           "case \"$opt\" in" n>
                           "\"?\")" n>
                           "echo \"Error! Unknown option $OPTARG\"" n>
                           "exit 2" n>
                           ";;" n>
                           "\":\")" n>
                           "echo \"Error! No argument value for option $OPTARG\"" n>
                           "exit 2" n>
                           ";;" n>
                           "esac" > n>
                           "done" > n>
                           )
                         "getopt"
                         "Parse command options"
                         'sh-tempo-tags))

(provide 'init-sh)
;;; init-sh.el ends here
