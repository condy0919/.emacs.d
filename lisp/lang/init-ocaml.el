;;; init-ocaml.el --- ocaml -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; Ocaml mode
(use-package tuareg
  :ensure t
  :mode ("\\.ml\\'" . tuareg-mode)
  :custom
  (tuareg-match-patterns-aligned t)
  (tuareg-indent-align-with-first-arg t))

;; Indentation tool for OCaml
;; Bundled with system package `ocaml-ocp-indent'
(use-package ocp-indent
  :ensure nil
  :when (executable-find "ocp-indent")
  :commands ocp-indent-region ocp-indent-buffer
  :hook (tuareg-mode . ocp-setup-indent))

;; The dune build system
;; Bundled with system package `dune'
(use-package dune
  :ensure nil
  :when (executable-find "dune")
  :mode ("dune\\(?:\\.inc\\)?\\'" . dune-mode))

(provide 'init-ocaml)

;;; init-ocaml.el ends here
