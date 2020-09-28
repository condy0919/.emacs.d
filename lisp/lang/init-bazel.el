;;; init-bazel.el --- bazel is the future -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package bazel-mode
  :ensure t
  :mode (("/WORKSPACE\\'"         . bazel-workspace-mode)
         ("/WORKSPACE\\.bazel\\'" . bazel-workspace-mode))
  :custom
  (bazel-mode-buildifier-before-save t))

;; Bundled with `bazel-mode'
(use-package bazel-build
  :ensure nil
  :defer t
  :commands (bazel-build bazel-run bazel-test))

(provide 'init-bazel)

;;; init-bazel.el ends here
