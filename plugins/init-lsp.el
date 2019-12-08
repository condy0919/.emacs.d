(use-package company
  :ensure t
  :defer t
  :init (global-company-mode)
  :bind (
     :map company-active-map
     ("C-p" . company-select-previous)
     ("C-n" . company-select-next)
     ("<tab>" . company-complete-common-or-cycle)
     :map company-search-map
     ("C-p" . company-select-previous)
     ("C-n" . company-select-next))
  :config
  (progn
    ;; Use Company for completion
    (bind-key [remap completion-at-point] #'company-complete company-mode-map)

    (setq company-tooltip-align-annotations t
          company-show-numbers t  ;; Easy navigation to candidates with M-<n>
          company-dabbrev-downcase nil
          company-idle-delay 0
          company-dabbrev-ignore-case nil
          )
    )
  :diminish company-mode)

(use-package company-quickhelp          ; Documentation popups for Company
  :ensure t
  :defer t
  :init (add-hook 'global-company-mode-hook #'company-quickhelp-mode))

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
  (setq lsp-enable-snippet nil
        lsp-clients-clangd-args '("-j=2"
                                  "--background-index"
                                  "--clang-tidy"
                                  "--suggest-missing-includes"
                                  "--completion-style=bundled"
                                  "--header-insertion=iwyu"
                                  "--pch-storage=memory"))
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-max-height 10
        lsp-ui-doc-max-width 40
        lsp-ui-sideline-ignore-duplicate t))

(use-package company-lsp
  :ensure t
  :config
  (setq company-lsp-enable-recompletion t))

;; lint 工具
(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode)
  :diminish " FC")

(provide 'init-lsp)
