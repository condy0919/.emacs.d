;;; init-bazel.el --- bazel is the future

;;; Commentary:
;;

;;; Code:

(use-package bazel-mode
  :ensure t
  :straight (:host github :repo "bazelbuild/emacs-bazel-mode")
  :defer t
  :config (setq bazel-mode-buildifier-before-save t))

(use-package bazel-build
  :ensure t
  :straight (:host github :repo "bazelbuild/emacs-bazel-mode")
  :defer t)

(provide 'init-bazel)

;;; init-bazel.el ends here
