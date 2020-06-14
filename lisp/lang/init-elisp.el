;;; init-elisp.el --- elisp -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:

(require 'init-core)

(use-package elisp-mode
  :ensure nil
  :bind (:map emacs-lisp-mode-map
         ;; consistent with inferior-python-shell
         ("C-c C-p" . my/ielm-other-window)
         ("C-c C-b" . eval-buffer)
         ("C-c C-c" . my/eval-to-comment)
         :map lisp-interaction-mode-map
         ("C-c C-c" . my/eval-to-comment))
  :config
  (defconst eval-as-comment-prefix ";;=> ")

  ;; Imitate scala-mode
  ;; from https://github.com/dakra/dmacs
  (defun my/eval-to-comment (&optional arg)
    (interactive "P")
    (let ((start (point)))
      (eval-print-last-sexp arg)
      (save-excursion
        (goto-char start)
        (save-match-data
          (re-search-forward "[[:space:]\n]+" nil t)
          (insert eval-as-comment-prefix))))))

(use-package ielm
  :ensure nil
  :hook (ielm-mode . my/buffer-auto-close))

(provide 'init-elisp)

;;; init-elisp.el ends here
