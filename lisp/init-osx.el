;;; init-osx.el --- Tweaks for MacOS -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; The builtin dictionary app in MacOS
(use-package osx-dictionary
  :ensure t
  :commands osx-dictionary-search-word-at-point)

(use-package emacs
  :ensure nil
  :bind (;; I heavily use frame in MacOS
         ("H-p" . ns-prev-frame)
         ("H-n" . ns-next-frame)
         ("H-g" . select-frame-by-name)
         ;; CUA for MacOS
         ("H-v" . clipboard-yank)
         ("H-c" . clipboard-kill-ring-save)
         ("H-x" . clipboard-kill-region))
  :config
  ;; Make titlebar dark
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  :custom
  (mac-option-modifier 'hyper)
  (mac-command-modifier 'meta)
  (delete-by-moving-to-trash t)
  ;; Curse Lion and its sudden but inevitable fullscreen mode!
  ;; NOTE Meaningless to railwaycat's emacs-mac build
  (ns-use-native-fullscreen nil)
  ;; Visit files opened outside of Emacs in existing frame, not a new one
  (ns-pop-up-frames nil))

(provide 'init-osx)
;;; init-osx.el ends here
