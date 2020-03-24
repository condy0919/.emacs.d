;;; init-lsp.el --- The completion engine and lsp client -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; The completion engine
(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :bind (:map company-mode-map
         ([remap completion-at-point] . company-complete)
         :map company-active-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next)
         ("C-s" . company-filter-candidates)
         ("<tab>" . company-complete-common-or-cycle)
         :map company-search-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next))
  :custom
  (company-idle-delay 0)
  (company-show-numbers t) ;; Easy navigation to candidates with M-<n>
  (company-minimum-prefix-length 4)
  (company-tooltip-align-annotations t)
  (company-backends '(company-capf
                      company-keywords
                      company-tempo))
  :config
  (with-eval-after-load 'evil
    ;; DONT show candidates menu when back to evil normal mode
    (add-hook 'evil-normal-state-entry-hook 'company-abort))
  )

;; Show docs when completion as an alternative for lsp-ui
(use-package company-quickhelp
  :ensure t
  :defines company-quickhelp-delay
  :bind (:map company-active-map
         ([remap company-show-doc-buffer] . company-quickhelp-manual-begin))
  :hook (company-mode . company-quickhelp-mode)
  :custom
  (company-quickhelp-use-propertized-text t)
  (company-quickhelp-max-lines 8))

;; Sorting & filtering
(use-package company-prescient
  :ensure t
  :hook ((company-mode . company-prescient-mode)
         (company-mode . prescient-persist-mode)))

;; lsp-mode
(use-package lsp-mode
  :ensure t
  :hook (prog-mode . lsp-deferred)
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-log-io nil)                     ;; enable log only for debug
  (lsp-enable-folding nil)             ;; use `evil-matchit' instead
  (lsp-diagnostic-package :flycheck)   ;; prefer flycheck
  (lsp-flycheck-live-reporting nil)    ;; obey `flycheck-check-syntax-automatically'
  (lsp-prefer-capf t)                  ;; using `company-capf' by default
  (lsp-enable-snippet nil)             ;; no snippet
  (lsp-enable-symbol-highlighting nil) ;; turn off for better performance
  (lsp-enable-on-type-formatting nil)  ;; disable formatting on the fly
  (lsp-auto-guess-root t)              ;; auto guess root
  (lsp-keep-workspace-alive nil)       ;; auto kill lsp server
  (lsp-eldoc-enable-hover nil)         ;; disable eldoc displays in minibuffer
  :bind (:map lsp-mode-map
         ("C-c f" . lsp-format-region)
         ("C-c d" . lsp-describe-thing-at-point)
         ("C-c a" . lsp-execute-code-action)
         ("C-c r" . lsp-rename))
  )

(provide 'init-lsp)

;;; init-lsp.el ends here
