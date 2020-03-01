;;; init-haskell.el --- haskell -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:

(use-package haskell-mode
  :ensure t
  :custom
  (haskell-process-check-cabal-config-on-load nil)
  (haskell-process-suggest-add-package nil)
  (haskell-process-suggest-haskell-docs-imports nil)
  (haskell-process-suggest-hoogle-imports nil)
  (haskell-process-suggest-language-pragmas nil)
  (haskell-process-suggest-no-warn-orphans nil)
  (haskell-process-suggest-overloaded-strings nil)
  (haskell-process-suggest-restart nil))

(use-package attrap
  :ensure t)

(use-package dante
  :ensure t
  :hook (haskell-mode . dante-mode)
  :config
  ;; Disable auto save flycheck
  (setq flymake-no-changes-timeout nil)
  (setq flymake-start-on-flymake-mode nil)
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
