;;; init-bazel.el --- bazel is the future -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package bazel-mode
  :ensure t
  :commands (bazel-build bazel-run bazel-test)
  :custom (bazel-mode-buildifier-before-save t)
  :mode (("WORKSPACE\\'" . bazel-mode)
         ("BUILD\\'"     . bazel-mode)
         ("\\.bazel\\'"  . bazel-mode)
         ("\\.bzl\\'"    . bazel-mode)
         ("\\.BUILD\\'"  . bazel-mode)))

(provide 'init-bazel)

;;; init-bazel.el ends here
