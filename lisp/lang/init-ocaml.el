;;; init-ocaml.el --- ocaml -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; Ocaml mode
(use-package tuareg
  :ensure t
  :mode ("\\.ml\\'" . tuareg-mode))

;; Indentation tool for OCaml
(use-package ocp-indent
  :ensure nil
  :commands (ocp-indent-region ocp-indent-buffer)
  :hook (tuareg-mode . ocp-setup-indent))

;; The dune build system
(use-package dune
  :ensure nil
  :mode ("dune\\(?:\\.inc\\)?\\'" . dune-mode))

(provide 'init-ocaml)

;;; init-ocaml.el ends here
