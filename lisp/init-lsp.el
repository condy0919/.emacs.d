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
         ;; The spacemacs binding style
         ("C-/"     . counsel-company)
         ("C-p"     . company-select-previous)
         ("C-n"     . company-select-next)
         ("C-s"     . company-filter-candidates)
         ([tab]     . company-complete-common-or-cycle)
         ([backtab] . company-select-previous-or-abort)
         :map company-search-map
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
  ;; make dabbrev-code case-sensitive
  (company-dabbrev-code-ignore-case nil)
  (company-dabbrev-code-everywhere t)
  (company-backends '(company-capf
                      company-files
                      (company-dabbrev-code company-etags company-keywords)
                      company-dabbrev)))

;; lsp-mode
(use-package lsp-mode
  :ensure t
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (prog-mode . lsp-deferred))
  :bind (:map lsp-mode-map
         ("C-c f" . lsp-format-region)
         ("C-c d" . lsp-describe-thing-at-point)
         ("C-c a" . lsp-execute-code-action)
         ("C-c r" . lsp-rename))
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-enable-links nil)                 ;; no clickable links
  (lsp-enable-folding nil)               ;; use `hideshow' instead
  (lsp-enable-snippet nil)               ;; no snippet
  (lsp-enable-file-watchers nil)         ;; turn off for better performance
  (lsp-enable-text-document-color nil)   ;; as above
  (lsp-enable-semantic-highlighting nil) ;; as above
  (lsp-enable-symbol-highlighting nil)   ;; as above
  (lsp-enable-indentation nil)           ;; indent by ourself
  (lsp-enable-on-type-formatting nil)    ;; disable formatting on the fly
  (lsp-modeline-code-actions-enable nil) ;; keep modeline clean
  (lsp-modeline-diagnostics-enable nil)  ;; as above
  (lsp-idle-delay 0.5)                   ;; lazy refresh
  (lsp-log-io nil)                       ;; enable log only for debug
  (lsp-diagnostics-provider :flycheck)   ;; prefer `flycheck'
  (lsp-lens-enable t)                    ;; enable lens
  (lsp-auto-guess-root t)                ;; auto guess root
  (lsp-keep-workspace-alive nil)         ;; auto kill lsp server
  (lsp-eldoc-enable-hover nil)           ;; disable eldoc hover
  (lsp-signature-auto-activate t)        ;; show function signature
  (lsp-signature-doc-lines 2))           ;; but dont take up more lines

;; Jump to workspace symbol
(use-package lsp-ivy
  :ensure t
  :commands my/lsp-ivy-workspace-symbol
  :config
  (defun my/lsp-ivy-workspace-symbol ()
    "A `lsp-ivy-workspace-symbol' wrapper that effective only in `lsp-mode'."
    (interactive)
    (when (bound-and-true-p lsp-mode)
      (call-interactively 'lsp-ivy-workspace-symbol))))

(provide 'init-lsp)

;;; init-lsp.el ends here
