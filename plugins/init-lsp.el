(use-package lsp-mode
  :ensure t
  :hook (prog-mode . lsp-deferred)
  :bind (:map lsp-mode-map
              ("C-c C-d" . lsp-describe-thing-at-point))
  :init
  (setq lsp-auto-guess-root t
        lsp-prefer-flymake nil
        flymake-fringe-indicator-position 'right-fringe)
  :config
  (setq lsp-enable-snippet nil)
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 35
        lsp-ui-sideline-ignore-duplicate t))

(use-package company-lsp
  :ensure t
  :config
  (setq company-lsp-enable-recompletion t))

(provide 'init-lsp)
