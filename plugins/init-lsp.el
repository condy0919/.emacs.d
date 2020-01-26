;;; init-lsp.el --- The completion engine and lsp client -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; The completion engine
(use-package company
  :ensure t
  :diminish company-mode
  :defines (company-dabbrev-downcase company-dabbrev-ignore-case)
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
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
        company-idle-delay 0.05
        company-minimum-prefix-length 2
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil)
  :diminish company-mode)

(use-package eglot
  :ensure t
  :hook ((rust-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (c++-mode . eglot-ensure))
  :bind (:map eglot-mode-map
              ("C-c d" . eglot-help-at-point)
              ("C-c f" . eglot-format)
              ("C-c a" . eglot-code-actions)
              ("C-c r" . eglot-rename))
  )

(provide 'init-lsp)

;;; init-lsp.el ends here
