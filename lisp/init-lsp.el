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
         ([escape] . company-abort)
         ;; consistent with ivy-occur
         ("C-c C-o" . counsel-company)
         ("C-p"     . company-select-previous)
         ("C-n"     . company-select-next)
         ("C-s"     . company-filter-candidates)
         ([tab]     . company-complete-common-or-cycle)
         ([backtab] . company-select-previous-or-abort)
         :map company-search-map
         ([escape] . company-search-abort)
         ("C-p"    . company-select-previous)
         ("C-n"    . company-select-next))
  :custom
  (company-idle-delay 0)
  (company-echo-delay 0)
  ;; Easy navigation to candidates with M-<n>
  (company-show-numbers t)
  (company-require-match nil)
  (company-minimum-prefix-length 3)
  (company-tooltip-align-annotations t)
  ;; complete `abbrev' only in current buffer
  (company-dabbrev-other-buffers nil)
  ;; make dabbrev case-sensitive
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-downcase nil)
  (company-backends '(company-capf
                      company-files
                      (company-dabbrev-code company-keywords)
                      company-dabbrev))
  :config
  (define-advice company-abort (:after nil)
    "Exit `evil-insert-state-mode' too."
    (when (bound-and-true-p evil-mode)
      (evil-force-normal-state)))
  (define-advice counsel-company (:after nil)
    "Back to `evil-insert-state' after `counsel-company' finishes."
    (when (bound-and-true-p evil-mode)
      (evil-insert-state)))
  )

;; lsp-mode
(use-package lsp-mode
  :ensure t
  :hook (prog-mode . lsp-deferred)
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-idle-delay 0.5)                 ;; lazy refresh
  (lsp-log-io nil)                     ;; enable log only for debug
  (lsp-enable-folding nil)             ;; use `hideshow' instead
  (lsp-enable-links nil)               ;; no clickable links
  (lsp-diagnostic-package :flycheck)   ;; prefer flycheck
  (lsp-lens-auto-enable t)             ;; enable lens
  (lsp-prefer-capf t)                  ;; using `company-capf' by default
  (lsp-enable-snippet nil)             ;; no snippet
  (lsp-enable-file-watchers nil)       ;; turn off for better performance
  (lsp-enable-text-document-color nil) ;; as above
  (lsp-enable-symbol-highlighting nil) ;; as above
  (lsp-enable-indentation nil)         ;; indent by ourself
  (lsp-enable-on-type-formatting nil)  ;; disable formatting on the fly
  (lsp-auto-guess-root t)              ;; auto guess root
  (lsp-keep-workspace-alive nil)       ;; auto kill lsp server
  (lsp-eldoc-enable-hover nil)         ;; disable eldoc hover
  (lsp-signature-auto-activate t)      ;; show function signature
  (lsp-signature-doc-lines 2)          ;; but dont take up more lines
  :bind (:map lsp-mode-map
         ("C-c f" . lsp-format-region)
         ("C-c d" . lsp-describe-thing-at-point)
         ("C-c a" . lsp-execute-code-action)
         ("C-c r" . lsp-rename))
  )

(provide 'init-lsp)

;;; init-lsp.el ends here
