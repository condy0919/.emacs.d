;;; init-hydra.el --- hydra tweaks -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package hydra
  :ensure t
  :after evil
  :bind ("C-c h" . hydra-copy/body)
  :config
  (defhydra hydra-other-window-scroll (nil nil)
    "Scroll in other window"
    ("j" scroll-other-window-line "down")
    ("k" scroll-other-window-down-line "up")
    ("C-f" scroll-other-window "down")
    ("C-b" scroll-other-window-down "up"))

  (defun scroll-other-window-line ()
    "Scroll up of one line in other window."
    (interactive)
    (scroll-other-window 1))

  (defun scroll-other-window-down-line ()
    "Scroll down of one line in other window."
    (interactive)
    (scroll-other-window-down 1))

  (defhydra hydra-copy (:color blue)
    "Copy"
    ("w" copy-word-at-point "word")
    ("s" copy-symbol-at-point "symbol")
    ("l" copy-line-at-point "line")
    ("u" copy-url-at-point "url")
    ("e" copy-email-at-point "email")
    ("r" copy-region "region"))

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

  (defun copy-region ()
    "Copy region."
    (interactive)
    (save-excursion
      (evil-avy-goto-char)
      (let* ((begin (point))
             (end (prog2
                      (evil-avy-goto-char)
                      (1+ (point)))))
        (copy-region-as-kill begin end))))
  )

(provide 'init-hydra)
;;; init-hydra.el ends here
