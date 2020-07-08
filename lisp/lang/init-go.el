;;; init-go.el --- go -*- lexical-binding: t -*-


;;; Commentary:
;;

;;; Code:

(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :config
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-envs '("GOPATH" "GOPROXY")))
  :custom
  (godoc-reuse-buffer t))

(provide 'init-go)

;;; init-go.el ends here
