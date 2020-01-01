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

;; 补全窗口弹出更快
(use-package company-posframe
  :ensure t
  :hook (company-mode . company-posframe-mode))

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
  :bind (:map lsp-ui-mode-map
         ("C-c C-a" . lsp-ui-sideline-apply-code-actions))
  :config
  (setq
    ;; doc
    lsp-ui-doc-max-height 30
    lsp-ui-doc-max-width 60
    lsp-ui-doc-header nil
    lsp-ui-doc-include-signature nil
    ;; sideline
    lsp-ui-sideline-enable t
    lsp-ui-sideline-ignore-duplicate t
    lsp-ui-sideline-show-hover t
    lsp-ui-sideline-show-symbol t
    lsp-ui-sideline-show-diagnostics t
    lsp-ui-sideline-show-code-actions t
    lsp-ui-sideline-code-action-prefix ""
    ;; flycheck
    lsp-ui-flycheck-enable t)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

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

(provide 'init-lsp)
