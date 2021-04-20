;;; init-bazel.el --- bazel is the future -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package bazel-mode
  :ensure t
  :commands bazel-build bazel-run bazel-test bazel-coverage
  :mode (("/WORKSPACE\\'"         . bazel-workspace-mode)
         ("/WORKSPACE\\.bazel\\'" . bazel-workspace-mode))
  :custom
  (bazel-mode-buildifier-before-save (executable-find "buildifier")))

(provide 'init-bazel)

;;; init-bazel.el ends here
