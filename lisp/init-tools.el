;;; init-tools.el --- We all like productive tools -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; Tips for next keystroke
(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode)
  :config
  (which-key-add-key-based-replacements
    "C-c !" "flycheck"
    "C-c @" "hideshow"
    "C-c i" "ispell"
    "C-c t" "hl-todo"
    "C-x a" "abbrev"
    "C-x n" "narrow"
    "C-x t" "tab")
  :custom
  (which-key-ellipsis "..")
  (which-key-separator " → ")
  (which-key-dont-use-unicode nil)
  (which-key-idle-delay 0.5)
  (which-key-add-column-padding 1))

;; The blazing grep tool
;;
;; Press C-c s to search
(use-package rg
  :ensure t
  :hook (after-init . rg-enable-default-bindings))

;; Jump to arbitrary positions
(use-package avy
  :ensure t
  ;; integrate with isearch and others
  ;; C-' to select isearch-candidate with avy
  :hook (after-init . avy-setup-default)
  :bind (("M-g M-l" . avy-goto-line)
         ("M-g M-j" . avy-goto-char-timer))
  :config
  (defun avy-action-embark-dwim (pt)
    (save-excursion
      (goto-char pt)
      (embark-dwim))
    t)
  :custom
  (avy-background t)
  (avy-all-windows nil)
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?q ?w ?e ?r ?u ?i ?o ?p))
  ;; overlay is used during isearch, `pre' style makes avy keys evident.
  (avy-styles-alist '((avy-isearch . pre)))
  (avy-dispatch-alist '((?y . avy-action-copy)
                        (?. . avy-action-embark-dwim))))

;; The builtin incremental search
(use-package isearch
  :ensure nil
  :bind (:map isearch-mode-map
         ;; consistent with ivy-occur
         ("C-c C-o"                   . isearch-occur)
         ([escape]                    . isearch-cancel)
         ;; Edit the search string instead of jumping back
         ([remap isearch-delete-char] . isearch-del-char))
  :config
  (define-advice isearch-occur (:after (_regexp &optional _nlines))
    (isearch-exit))
  :custom
  ;; Record isearch in minibuffer history, so C-x ESC ESC can repeat it.
  (isearch-resume-in-command-history t)
  ;; One space can represent a sequence of whitespaces
  (isearch-lax-whitespace t)
  (isearch-regexp-lax-whitespace t)
  (isearch-repeat-on-direction-change t)
  ;; M-< and M-> move to the first/last occurrence of the current search string.
  (isearch-allow-motion t)
  (isearch-motion-changes-direction t)
  ;; lazy isearch
  (isearch-lazy-count t)
  (isearch-lazy-highlight t)
  (lazy-count-prefix-format nil)
  (lazy-count-suffix-format " [%s/%s]")
  (lazy-highlight-buffer t)
  ;; Mimic Vim
  (lazy-highlight-cleanup nil))

;; Writable grep buffer
(use-package wgrep
  :ensure t
  :hook (grep-setup . wgrep-setup)
  :custom
  (wgrep-change-readonly-file t))

;; GC optimization
(use-package gcmh
  :ensure t
  :hook (after-init . gcmh-mode)
  :custom
  (gcmh-idle-delay 10)
  (gcmh-high-cons-threshold #x6400000)) ;; 100 MB

;; Write documentation comment in an easy way
(use-package separedit
  :ensure t
  :bind (:map prog-mode-map
         ("C-c '" . separedit))
  :custom
  (separedit-default-mode 'markdown-mode)
  (separedit-remove-trailing-spaces-in-comment t)
  (separedit-continue-fill-column t)
  (separedit-buffer-creation-hook #'auto-fill-mode))

;; Universal menus
(use-package transient
  :ensure nil
  :bind (("C-c h o" . scroll-other-window-menu)
         ("C-c h t" . background-opacity-menu))
  :config
  (transient-define-prefix scroll-other-window-menu ()
    "Scroll other window."
    :transient-suffix     'transient--do-stay
    [["Line"
      ("j" "next line" scroll-other-window-line)
      ("k" "previous line" scroll-other-window-down-line)]
     ["Page"
      ("C-f" "next page" scroll-other-window)
      ("C-b" "previous page" scroll-other-window-down)]])

  (defun scroll-other-window-line ()
    "Scroll up of one line in other window."
    (interactive)
    (scroll-other-window 1))

  (defun scroll-other-window-down-line ()
    "Scroll down of one line in other window."
    (interactive)
    (scroll-other-window-down 1))

  (transient-define-prefix background-opacity-menu ()
    "Set frame background opacity."
    [:description
     background-opacity-get-alpha-str
     ("+" "increase" background-opacity-inc-alpha :transient t)
     ("-" "decrease" background-opacity-dec-alpha :transient t)
     ("=" "set to ?" background-opacity-set-alpha)])

  (defun background-opacity-inc-alpha (&optional n)
    (interactive)
    (let* ((alpha (background-opacity-get-alpha))
           (next-alpha (cl-incf alpha (or n 1))))
      (set-frame-parameter nil 'alpha-background next-alpha)))

  (defun background-opacity-dec-alpha ()
    (interactive)
    (background-opacity-inc-alpha -1))

  (defun background-opacity-set-alpha (alpha)
    (interactive "nSet to: ")
    (set-frame-parameter nil 'alpha-background alpha))

  (defun background-opacity-get-alpha ()
    (pcase (frame-parameter nil 'alpha-background)
      ((pred (not numberp)) 100)
      (`,alpha alpha)))

  (defun background-opacity-get-alpha-str ()
    (format "Alpha %s%%" (background-opacity-get-alpha))))

;; Pastebin service
(use-package webpaste
  :ensure t
  :commands webpaste-paste-buffer-or-region
  :custom
  (webpaste-open-in-browser t)
  (webpaste-paste-confirmation t)
  (webpaste-add-to-killring nil)
  (webpaste-provider-priority '("paste.mozilla.org" "dpaste.org" "ix.io")))

;; Web search
(use-package webjump
  :ensure nil
  ;; C-c / will be shadowed by `org-sparse-tree' in org-mode
  :bind ("C-c C-/" . webjump)
  :config
  (defconst webjump-weather-default-cities '("杭州" "深圳" "北京" "上海"))
  (defconst webjump-weather-url-template "https://weathernew.pae.baidu.com/weathernew/pc?query=%s天气&srcid=4982")

  (defun webjump-weather (_name)
    (let ((city (completing-read "City: " webjump-weather-default-cities)))
      (format webjump-weather-url-template city)))

  (add-to-list 'browse-url-handlers '("weathernew.pae.baidu.com" . xwidget-webkit-browse-url))
  :custom
  (webjump-sites '(;; Internet search engines.
                   ("Google" .
                    [simple-query "www.google.com"
                                  "www.google.com/search?q=" ""])
                   ("Wikipedia" .
                    [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""])
                   ("Ludwig Guru" .
                    [simple-query "ludwig.guru" "ludwig.guru/s/" ""])
                   ("Stack Overflow" .
                    [simple-query "stackoverflow.com" "stackoverflow.com/search?q=" ""])
                   ("Man Search" .
                    [simple-query "archlinux.org" "man.archlinux.org/search?q=" ""])
                   ("Man Go" .
                    [simple-query "archlinux.org" "man.archlinux.org/search?q=" "&go=Go"])

                   ;; Code search
                   ("Code Search" .
                    [simple-query "sourcegraph.com" "sourcegraph.com/search?q=context:global+" "&patternType=literal"])

                   ;; Life
                   ("Weather" . webjump-weather)

                   ;; Language specific engines.
                   ("x86 Instructions Reference" .
                    [simple-query "www.felixcloutier.com"
                                  "www.felixcloutier.com/x86/" ""]))))

;; Translator for Emacs
;; M-x fanyi-dwim{,2}, that's all.
(use-package fanyi
  :ensure t
  :commands fanyi-dwim fanyi-dwim2)

;; Edit text for browser with GhostText or AtomicChrome extension
(use-package atomic-chrome
  :ensure t
  :hook ((emacs-startup . atomic-chrome-start-server)
         (atomic-chrome-edit-mode . delete-other-windows))
  :custom
  (atomic-chrome-buffer-open-style 'frame)
  (atomic-chrome-default-major-mode 'markdown-mode)
  (atomic-chrome-url-major-mode-alist '(("github\\.com" . gfm-mode))))

;; IRC client
(use-package rcirc
  :ensure nil
  :hook (rcirc-mode . rcirc-omit-mode)
  :config
  (with-no-warnings
    (defun rcirc-notify-me (proc sender _response target text)
      "Notify me if SENDER sends a TEXT that matches my nick."
      (when (and (not (string= (rcirc-nick proc) sender))        ;; Skip my own message
                 (not (string= (rcirc-server-name proc) sender)) ;; Skip the response of server
                 (rcirc-channel-p target))
        (when (string-match (rcirc-nick proc) text)
          (notify-send :title (format "%s mention you" sender)
                       :body text
                       :urgency 'critical))))

    (add-hook 'rcirc-print-functions #'rcirc-notify-me))
  :custom
  (rcirc-default-port 7000)
  (rcirc-kill-channel-buffers t)
  ;; Always cycle for completions
  (rcirc-cycle-completion-flag t)
  (rcirc-auto-authenticate-flag t)
  (rcirc-authenticate-before-join t)
  (rcirc-fill-column #'window-text-width)
  ;; print messages in current channel buffer
  (rcirc-always-use-server-buffer-flag nil))

(provide 'init-tools)
;;; init-tools.el ends here
