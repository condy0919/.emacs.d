;; common
;; Compilation Mode
(use-package compile
  :ensure nil
  :preface
  ;; ANSI Coloring
  ;; @see https://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode
  (defun my-colorize-compilation-buffer ()
    "ANSI coloring in compilation buffers."
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :hook (compilation-filter . my-colorize-compilation-buffer))

;; 高亮 TODO
(use-package hl-todo
  :ensure t
  :bind (:map hl-todo-mode-map
              ([C-f3] . hl-todo-occur)
              ("C-c t p" . hl-todo-previous)
              ("C-c t n" . hl-todo-next)
              ("C-c t o" . hl-todo-occur))
  :hook (after-init . global-hl-todo-mode)
  :config
  (dolist (keyword '("BUG" "ISSUE" "FIXME" "XXX" "NOTE" "NB"))
    (cl-pushnew `(,keyword . ,(face-foreground 'error)) hl-todo-keyword-faces))
  (dolist (keyword '("WORKAROUND" "HACK" "TRICK"))
    (cl-pushnew `(,keyword . ,(face-foreground 'warning)) hl-todo-keyword-faces)))

;; C/C++
(use-package cc-mode
  :ensure nil
  :hook (c-mode-common . (lambda ()
                           (c-set-style "stroustrup")
                           (setq tab-width 4
                                 c-basic-offset 4))))

(use-package modern-cpp-font-lock
  :ensure t
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package clang-format
  :ensure t
  :commands (clang-format-region)
  :bind (:map c-mode-base-map
              ("C-c f" . clang-format-region))
)

;; rust
(use-package rust-mode
  :ensure t
  :config
;;  (setq rust-format-on-save t)
  (use-package cargo
    :ensure t
    :diminish cargo-minor-mode
    :hook (rust-mode . cargo-minor-mode)))

(use-package flycheck-rust
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  (add-hook 'rust-mode-hook 'flycheck-mode))

;; bazel
(use-package bazel-mode
  :ensure t
  :hook (bazel-mode . (lambda () (add-hook 'before-save-hook #'bazel-format nil t))))

(provide 'init-dev)
