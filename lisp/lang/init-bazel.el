;;; init-bazel.el --- bazel is the future -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package bazel-mode
  :ensure t
  :mode (("WORKSPACE\\'" . bazel-mode)
         ("BUILD\\'"     . bazel-mode)
         ("\\.bazel\\'"  . bazel-mode)
         ("\\.bzl\\'"    . bazel-mode)
         ("\\.BUILD\\'"  . bazel-mode))
  :straight (:host github :repo "bazelbuild/emacs-bazel-mode")
  :config (setq bazel-mode-buildifier-before-save t))

(use-package bazel-build
  :ensure t
  :defer 1
  :straight (:host github :repo "bazelbuild/emacs-bazel-mode"))

(provide 'init-bazel)

;;; init-bazel.el ends here
