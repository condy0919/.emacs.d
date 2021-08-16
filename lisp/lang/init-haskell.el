;;; init-haskell.el --- haskell -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:

(use-package haskell-mode
  :ensure t
  :hook ((haskell-mode . haskell-indentation-mode)
         (haskell-mode . haskell-doc-mode))
  :custom
  (haskell-completing-read-function 'completing-read)
  (haskell-process-check-cabal-config-on-load nil)
  (haskell-process-suggest-add-package nil)
  (haskell-process-suggest-hoogle-imports nil)
  (haskell-process-suggest-language-pragmas nil)
  (haskell-process-suggest-overloaded-strings nil)
  (haskell-process-suggest-restart nil))

(provide 'init-haskell)
;;; init-haskell.el ends here
