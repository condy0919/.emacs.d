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
;; no indentation inside namespace
(use-package cc-mode
  :ensure nil
  :hook (c-mode-common . (lambda ()
                           (c-set-offset 'innamespace [0])
                           (setq c-basic-offset 4
                                 tab-width 4))))

(use-package modern-cpp-font-lock
  :ensure t
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package clang-format
  :ensure t
  :commands (clang-format-region)
  :bind (:map c-mode-base-map
              ("C-c f" . clang-format-region)))

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
  ;;:ensure t
  :load-path "/home/condy/.emacs.d/localtest"
  :hook (bazel-mode . (lambda () (add-hook 'before-save-hook #'bazel-format nil t))))

;; show trailing whitespaces
(use-package whitespace
  :ensure nil
  :hook ((prog-mode markdown-mode conf-mode) . whitespace-mode)
  :config
  (setq whitespace-style '(face trailing)))

;; quickrun codes, including cpp. awesome!
(use-package quickrun
  :ensure t
  :bind (("C-c r" . quickrun)))

(provide 'init-dev)
