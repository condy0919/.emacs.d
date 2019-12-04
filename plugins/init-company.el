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

(provide 'init-company)
