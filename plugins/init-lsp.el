(use-package company
  :ensure t
  :defer t
  :hook (prog-mode . company-mode)
  :bind (
     :map company-active-map
     ("C-p" . company-select-previous)
     ("C-n" . company-select-next)
     ("<tab>" . company-complete-common-or-cycle)
     :map company-search-map
     ("C-p" . company-select-previous)
     ("C-n" . company-select-next))
  :config
  ;; Use Company for completion
  (bind-key [remap completion-at-point] #'company-complete company-mode-map)

  (setq company-tooltip-align-annotations t
        company-show-numbers t  ;; Easy navigation to candidates with M-<n>
        company-dabbrev-downcase nil
        company-idle-delay 0.05
        company-dabbrev-ignore-case nil)
  :diminish company-mode)

(use-package lsp-mode
  :ensure t
  :hook (prog-mode . lsp-deferred)
  :init
  (setq lsp-auto-guess-root t
        lsp-prefer-flymake nil
        flymake-fringe-indicator-position 'right-fringe)
  :config
  (setq lsp-enable-snippet nil
        lsp-clients-clangd-args '("-j=2"
                                  "--background-index"
                                  "--clang-tidy"
                                  "--suggest-missing-includes"
                                  "--completion-style=bundled"
                                  "--pch-storage=memory"))
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-max-height 10
        lsp-ui-doc-max-width 40
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-doc-header nil
        lsp-ui-doc-include-signature nil))

(use-package company-lsp
  :ensure t
  :after lsp-mode
  :config
  (setq company-transformers nil
        company-lsp-cache-candidates 'auto))

(use-package lsp-ivy
  :ensure t
  :after lsp-mode
  :bind (:map lsp-mode-map
              ([remap xref-find-apropos] . lsp-ivy-workspace-symbol)))

;; lint 工具
(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode)
  :diminish " FC")

(use-package flycheck-posframe
  :ensure t
  :hook (flycheck-mode . flycheck-posframe-mode))

(provide 'init-lsp)
