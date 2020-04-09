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
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next)
         ("C-s" . company-filter-candidates)
         ("<tab>" . company-complete-common-or-cycle)
         :map company-search-map
         ([escape] . company-search-abort)
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next))
  :custom
  (company-idle-delay 0)
  (company-echo-delay 0)
  (company-show-numbers t) ;; Easy navigation to candidates with M-<n>
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
                      company-dabbrev)))

;; lsp-mode
(use-package lsp-mode
  :ensure t
  :hook (prog-mode . lsp-deferred)
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-idle-delay 0.5)                 ;; lazy refresh
  (lsp-log-io nil)                     ;; enable log only for debug
  (lsp-enable-folding nil)             ;; use `evil-matchit' instead
  (lsp-diagnostic-package :flycheck)   ;; prefer flycheck
  (lsp-flycheck-live-reporting nil)    ;; obey `flycheck-check-syntax-automatically'
  (lsp-prefer-capf t)                  ;; using `company-capf' by default
  (lsp-enable-snippet nil)             ;; no snippet
  (lsp-enable-file-watchers nil)       ;; turn off for better performance
  (lsp-enable-text-document-color nil) ;; as above
  (lsp-enable-symbol-highlighting nil) ;; as above
  (lsp-enable-indentation nil)         ;; indent by ourself
  (lsp-enable-on-type-formatting nil)  ;; disable formatting on the fly
  (lsp-auto-guess-root t)              ;; auto guess root
  (lsp-keep-workspace-alive nil)       ;; auto kill lsp server
  (lsp-eldoc-enable-hover nil)         ;; disable eldoc displays in minibuffer
  (lsp-signature-auto-activate t)      ;; show function signature
  (lsp-signature-doc-lines 1)          ;; but dont take up more spaces
  :bind (:map lsp-mode-map
         ("C-c f" . lsp-format-region)
         ("C-c d" . lsp-describe-thing-at-point)
         ("C-c a" . lsp-execute-code-action)
         ("C-c r" . lsp-rename))
  )

(provide 'init-lsp)

;;; init-lsp.el ends here
