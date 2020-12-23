;;; init-hydra.el --- hydra tweaks -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package hydra
  :ensure t
  :after evil
  :bind (("C-c h c" . hydra-copy/body)
         ("C-c h m" . hydra-macro/body)
         ("C-c h w" . hydra-other-window-scroll/body))
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

  (defhydra hydra-macro (nil nil)
    "
    ^Create-Cycle^   ^Basic^           ^Insert^        ^Save^         ^Edit^
  ╭─────────────────────────────────────────────────────────────────────────╯
       ^_k_^           [_e_] execute    [_i_] insert    [_b_] name      [_'_] previous
       ^^↑^^           [_d_] delete     [_t_] set       [_K_] key       [_,_] last
   ^_(_^ ←   → ^_)_^       [_o_] edit       [_a_] add       [_x_] register
       ^^↓^^           [_r_] region     [_f_] format    [_B_] defun
       ^_j_^           [_m_] step
      ^^   ^^          [_s_] swap
"
    ("(" kmacro-start-macro :color blue)
    (")" kmacro-end-or-call-macro-repeat)
    ("k" kmacro-cycle-ring-previous)
    ("j" kmacro-cycle-ring-next)
    ("r" apply-macro-to-region-lines)
    ("d" kmacro-delete-ring-head)
    ("e" kmacro-end-or-call-macro-repeat)
    ("o" kmacro-edit-macro-repeat)
    ("m" kmacro-step-edit-macro)
    ("s" kmacro-swap-ring)
    ("i" kmacro-insert-counter)
    ("t" kmacro-set-counter)
    ("a" kmacro-add-counter)
    ("f" kmacro-set-format)
    ("b" kmacro-name-last-macro)
    ("K" kmacro-bind-to-key)
    ("B" insert-kbd-macro)
    ("x" kmacro-to-register)
    ("'" kmacro-edit-macro)
    ("," edit-kbd-macro))

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
