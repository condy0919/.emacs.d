;;; init-spell.el --- Spell checker -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:

;; Interactive spell checker
;;
;; z= `ispell-word'
(use-package ispell
  :ensure nil
  :bind (("C-c i c" . ispell-comments-and-strings)
         ("C-c i p" . ispell-comment-or-string-at-point)
         ("C-c i k" . ispell-kill-ispell)
         ("C-c i m" . ispell-message)
         ("C-c i b" . ispell-buffer)
         ("C-c i r" . ispell-region))
  :config
  ;; MacOS is broken
  (when (eq system-type 'darwin)
    (setenv "DICTIONARY" "en_US"))

  ;; no spell checking for org special blocks
  (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
  (add-to-list 'ispell-skip-region-alist '("#\\+begin_src" . "#\\+end_src"))
  (add-to-list 'ispell-skip-region-alist '("#\\+begin_example" . "#\\+end_example"))
  :custom
  (ispell-really-hunspell t)
  (ispell-program-name "hunspell")
  (ispell-dictionary "en_US")
  (ispell-following-word t)
  (ispell-personal-dictionary (locate-user-emacs-file "hunspell_dict.txt")))

;; Spell check on-the-fly
(use-package flyspell
  :ensure nil
  :custom
  ;; Use M-C-i instead if M-TAB is shadowed by your window manager
  (flyspell-use-meta-tab t)
  (flyspell-issue-welcome-flag nil)
  (flyspell-issue-message-flag nil))

(provide 'init-spell)
;;; init-spell.el ends here
