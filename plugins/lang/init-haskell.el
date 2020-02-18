;;; init-haskell.el --- haskell -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:

(use-package haskell-mode
  :ensure t)

(use-package dante
  :ensure t
  :hook (haskell-mode . dante-mode)
  :config
  ;; Disable auto save flycheck
  (setq flymake-no-changes-timeout nil)
  (setq flymake-start-syntax-check-on-newline nil)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  :bind (:map haskell-mode-map
         ;; Compatible with lsp-mode keybindings
         ("C-c d" . dante-info)
         ("C-c a" . attrap-attrap)
         ("C-c C-c" . dante-eval-block))
  )

;; pretty printer
(use-package hindent
  :ensure t
  :hook (haskell-mode . hindent-mode)
  :custom
  (hindent-process-path "~/.local/bin/hindent"))

(provide 'init-haskell)

;;; init-haskell.el ends here
