;;; init-ocaml.el --- ocaml -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; ocaml mode
(use-package tuareg
  :ensure t)

;; Indentation tool for OCaml
(use-package ocp-indent
  :ensure t
  :bind (:map tuareg-mode-map
         ("C-c f" . ocp-indent-region)))

;; Context sensitive completion
(use-package merlin
  :ensure t
  :hook (tuareg-mode . merlin-mode)
  :custom
  (merlin-error-after-save nil)
  (merlin-command "ocamlmerlin")
  (merlin-completion-with-doc t))

;; dune build system
(use-package dune
  :ensure t)

;; DONT forget to install https://github.com/reasonml/reason-cli
;; ocaml dialect for web
(use-package reason-mode
  :ensure t)

(provide 'init-ocaml)

;;; init-ocaml.el ends here
