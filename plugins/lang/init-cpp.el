;;; init-cpp.el --- Cpp
;; no indentation inside namespace

;;; Commentary:
;;

;;; Code:

(use-package cc-mode
  :ensure nil
  :hook (c-mode-common . (lambda ()
                           ;; no indentation after namespace
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

(provide 'init-cpp)

;;; init-cpp.el ends here
