;;; init-elisp.el --- elisp -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:

(use-package elisp-mode
  :ensure nil
  :bind (:map emacs-lisp-mode-map
         ("C-c C-c" . eval-to-comment)
         :map lisp-interaction-mode-map
         ("C-c C-c" . eval-to-comment))
  :config
  (defun eval-to-comment (&optional arg)
    "Evaluate the sexp before point and insert the result as comments.
Prefix argument ARG is forwarded to `eval-last-sexp' (treating
`-' as normal insertion, e.g., 0 disables truncation)."
    (interactive "P")
    (let* ((buf (current-buffer))
           (start (copy-marker (point)))
           (result
            (unwind-protect
                (progn
                  (eval-last-sexp (if (memq arg '(nil -)) t arg))
                  (when (and (eq (current-buffer) buf)
                             (> (point) start))
                    (delete-and-extract-region start (point))))
              (set-marker start nil))))
      (when result
        (end-of-line)
        (let ((beg (1+ (point)))
              (end (make-marker)))
          (set-marker-insertion-type end t)
          (unwind-protect
              (progn
                (insert "\n" result)
                (set-marker end (point))
                (unless (bolp) (insert "\n"))
                (let ((comment-empty-lines t))
                  (comment-region beg (point)))
                ;; Drop the temporary trailing newline used by `comment-region'.
                (delete-char -1)
                (goto-char beg)
                (while (< (point) end)
                  (indent-according-to-mode)
                  (forward-line 1))
                (goto-char end))
            (set-marker end nil)))))))

(use-package ielm
  :ensure nil
  :hook (ielm-mode . company-mode))

(provide 'init-elisp)

;;; init-elisp.el ends here
