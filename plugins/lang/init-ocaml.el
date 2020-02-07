;;; init-ocaml.el --- ocaml -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package tuareg
  :ensure t)

(use-package merlin
  :ensure t
  :hook (tuareg-mode . merlin-mode)
  :custom
  (merlin-error-after-save nil))

(use-package dune
  :ensure t)

(provide 'init-ocaml)

;;; init-ocaml.el ends here
