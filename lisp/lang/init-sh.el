;;; init-sh.el --- Shell scripts -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; Edit shell scripts
;;
;; awesoome. sh-mode provides `sh-while-getopts' to automate getopts.
;;
;; Emacs master enables `sh-mode' when editing archlinux PKGBUILD.
;; See https://debbugs.gnu.org/cgi/bugreport.cgi?bug=46660
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

(provide 'init-sh)
;;; init-sh.el ends here
