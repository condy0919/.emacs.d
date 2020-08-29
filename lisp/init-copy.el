;;; init-copy.el --- copy thing while editing -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package hydra
  :ensure t
  :after evil
  :bind ("C-c h" . hydra-copy/body)
  :config
  (defhydra hydra-copy (:color blue)
    "Copy"
    ("w" copy-word-at-point "word")
    ("s" copy-symbol-at-point "symbol")
    ("l" copy-line-at-point "line")
    ("u" copy-url-at-point "url")
    ("e" copy-email-at-point "email")
    ("q" nil "cancel"))

  (defun copy-word-at-point ()
    "Copy word at point."
    (interactive)
    (save-excursion
      (evil-avy-goto-word-or-subword-1)
      (kill-new (thing-at-point 'word))))

  (defun copy-symbol-at-point ()
    "Copy symbol at point."
    (interactive)
    (save-excursion
      (evil-avy-goto-symbol-1)
      (kill-new (thing-at-point 'symbol))))

  (defun copy-line-at-point ()
    "Copy line at point."
    (interactive)
    (save-excursion
      (evil-avy-goto-line)
      (kill-new (thing-at-point 'line))))

  (defun copy-url-at-point ()
    "Copy url at point."
    (interactive)
    (save-excursion
      (evil-avy-goto-word-or-subword-1)
      (kill-new (thing-at-point 'url))))

  (defun copy-email-at-point ()
    "Copy email at point."
    (interactive)
    (save-excursion
      (evil-avy-goto-word-or-subword-1)
      (kill-new (thing-at-point 'email))))
  )

(provide 'init-copy)
;;; init-copy.el ends here
