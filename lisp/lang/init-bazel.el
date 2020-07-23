;;; init-bazel.el --- bazel is the future -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package bazel-mode
  :ensure t
  :mode (("bazel\\.bazelrc\\'"   . bazelrc-mode)
         ("bazel\\.rc\\'"        . bazelrc-mode)
         ("\\.bazelrc\\'"        . bazelrc-mode)
         ("\\.bzl\\'"            . bazel-starlark-mode)
         ("WORKSPACE\\'"         . bazel-workspace-mode)
         ("WORKSPACE\\.bazel\\'" . bazel-workspace-mode)
         ("BUILD\\'"             . bazel-build-mode)
         ("BUILD.bazel\\'"       . bazel-build-mode))
  :custom (bazel-mode-buildifier-before-save t))

;; Bundled with `bazel-mode'
(use-package bazel-build
  :ensure nil
  :defer t
  :commands (bazel-build bazel-run bazel-test))

(provide 'init-bazel)

;;; init-bazel.el ends here
