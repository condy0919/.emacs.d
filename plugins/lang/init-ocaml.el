;;; init-ocaml.el --- ocaml -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; Ocaml mode
(use-package tuareg
  :ensure t
  :mode ("\\.ml\\'" . tuareg-mode)
  :bind (:map tuareg-mode-map
         ;; consistent with lsp-mode
         ("C-c f" . ocp-indent-region)
         ("C-c d" . merlin-type-enclosing)))

;; Indentation tool for OCaml
(use-package ocp-indent
  :ensure t
  :commands (ocp-indent-region))

;; Context sensitive completion
(use-package merlin
  :ensure t
  :hook (tuareg-mode . merlin-mode)
  :commands (merlin-type-enclosing)
  :custom
  (merlin-error-after-save nil)
  (merlin-command "ocamlmerlin")
  (merlin-completion-with-doc t))

;; Dune build system
(use-package dune
  :ensure t)

(provide 'init-ocaml)

;;; init-ocaml.el ends here
