;;; init-lsp.el --- The completion engine and lsp client -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; The completion engine
(use-package company
  :ensure t
  :diminish company-mode
  :defines (company-dabbrev-downcase company-dabbrev-ignore-case company-clang-arguments)
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
        company-idle-delay 0.1
        company-minimum-prefix-length 2
        company-clang-arguments '("-std=c++17")
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil)
  :diminish company-mode)

;; Show docs when completion as an alternative for lsp-ui
(use-package company-quickhelp
  :ensure t
  :defines company-quickhelp-delay
  :bind (:map company-active-map
         ([remap company-show-doc-buffer] . company-quickhelp-manual-begin))
  :hook (company-mode . company-quickhelp-mode)
  :custom
  (company-quickhelp-use-propertized-text t)
  (company-quickhelp-delay 0.1)
  (company-quickhelp-max-lines 8))

;; Sorting & filtering
(use-package company-prescient
  :ensure t
  :hook (company-mode . company-prescient-mode)
  :config (prescient-persist-mode +1))

;; lsp-mode
(use-package lsp-mode
  :ensure t
  :hook (prog-mode . lsp-deferred)
  :init (setq flymake-fringe-indicator-position 'right-fringe)
  :custom
  (lsp-log-io nil)                     ;; enable log only for debug
  (lsp-enable-folding nil)             ;; use `evil-matchit' instead
  (lsp-diagnostic-package :flycheck)   ;; prefer flycheck
  (lsp-prefer-capf t)                  ;; using `company-capf' by default
  (lsp-enable-snippet nil)             ;; no snippet
  (lsp-enable-symbol-highlighting nil) ;; turn off for better performance
  (lsp-enable-on-type-formatting nil)  ;; Disable formatting on the fly
  (lsp-auto-guess-root t)              ;; auto guess root
  (lsp-keep-workspace-alive nil)       ;; auto kill lsp server
  (lsp-eldoc-enable-hover nil)         ;; Disable eldoc displays in minibuffer
  :bind (:map lsp-mode-map
              ("C-c f" . lsp-format-region)
              ("C-c d" . lsp-describe-thing-at-point)
              ("C-c a" . lsp-execute-code-action)
              ("C-c r" . lsp-rename))
  )

(provide 'init-lsp)

;;; init-lsp.el ends here
