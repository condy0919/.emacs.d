;;; init-tools.el --- We all like productive tools -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; Tips for next keystroke
(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode)
  :config
  (dolist (k '(("C-c !" "flycheck")
               ("C-c @" "hideshow")
               ("C-c i" "ispell")
               ("C-c n" "org-roam")
               ("C-c t" "hl-todo")
               ("C-x a" "abbrev")
               ("C-x n" "narrow")))
    (cl-destructuring-bind (key name) k
      (which-key-add-key-based-replacements key name)))
  :custom
  (which-key-idle-delay 0.5)
  (which-key-add-column-padding 1))

;; The blazing grep tool
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
  :config
  ;; Force to use pre `avy-style'
  (define-advice avy-isearch (:around (func &rest args))
    (let ((avy-style 'pre))
      (apply func args)))
  :custom
  (avy-timeout-seconds 0.2)
  (avy-all-windows nil)
  (avy-background t)
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?q ?w ?e ?r ?u ?i ?o ?p)))

;; ivy core
(use-package ivy
  :ensure t
  :hook (after-init . ivy-mode)
  :bind (("C-c C-r" . ivy-resume)
         :map ivy-minibuffer-map
         ("C-c C-e" . ivy-woccur)
         ("C-w"     . ivy-yank-word)
         :map ivy-occur-mode-map
         ("C-c C-e" . ivy-wgrep-change-to-wgrep-mode)
         :map ivy-occur-grep-mode-map
         ("C-c C-e" . ivy-wgrep-change-to-wgrep-mode))
  :config
  ;; Bring C-' back
  (use-package ivy-avy
    :ensure t)

  (use-package ivy-hydra
    :ensure t)

  ;; Copy from
  ;; https://github.com/honmaple/maple-emacs/blob/master/lisp/init-ivy.el
  (defun ivy-woccur ()
    "ivy-occur with wgrep-mode enabled."
    (interactive)
    (run-with-idle-timer 0 nil 'ivy-wgrep-change-to-wgrep-mode)
    (ivy-occur))
  :custom
  (ivy-display-style 'fancy)           ;; fancy style
  (ivy-count-format "%d/%d ")          ;; better counts
  (ivy-use-virtual-buffers t)          ;; show recent files
  (ivy-extra-directories '("./"))      ;; no ".." directories
  (ivy-height 10)
  (ivy-sort-max-size 3000)             ;; the default value 30000 is too large
  (ivy-fixed-height-minibuffer t)      ;; fixed height
  (ivy-on-del-error-function 'ignore)) ;; dont quit minibuffer when del-error

;; Fuzzy matcher
(use-package counsel
  :ensure t
  :hook (ivy-mode . counsel-mode)
  :bind (([remap evil-ex-registers]  . counsel-evil-registers)
         ([remap evil-show-marks]    . counsel-mark-ring)
         ([remap evil-show-jumps]    . evil-jump-list)
         ([remap recentf-open-files] . counsel-recentf)
         ([remap swiper]             . counsel-grep-or-swiper)
         ("M-y"                      . counsel-yank-pop))
  :config
  (ivy-set-actions
   'counsel-find-file
   '(("d" my/delete-file "delete")
     ("r" my/rename-file "rename")
     ("l" vlf            "view large file")
     ("b" hexl-find-file "open file in binary mode")
     ("x" counsel-find-file-as-root "open as root")))

  ;; Modified from doom
  (defun evil-jump-list ()
    "evil jump list with ivy enhancement."
    (interactive)
    (ivy-read "Jump: "
              (nreverse
               (delete-dups
                (mapcar (lambda (mark)
                          (cl-destructuring-bind (pt path) mark
                            (let ((buf (get-file-buffer path)))
                              (unless buf
                                (setq buf (find-file-noselect path t)))
                              (with-current-buffer buf
                                (goto-char pt)
                                (font-lock-fontify-region (line-beginning-position) (line-end-position))
                                (cons (format "%s:%d %s"
                                              (buffer-name)
                                              (line-number-at-pos)
                                              (string-trim-right (or (thing-at-point 'line) "")))
                                      (point-marker))))))
                        (evil--jumps-savehist-sync))))
              :sort nil
              :require-match t
              :action (lambda (cand)
                        (let ((mark (cdr cand)))
                          (with-current-buffer (switch-to-buffer (marker-buffer mark))
                            (goto-char (marker-position mark)))))))
  :custom
  (counsel-preselect-current-file t)
  (counsel-yank-pop-preselect-last t)
  (counsel-yank-pop-separator "\n-----------\n")
  (counsel-find-file-at-point t)
  (counsel-find-file-ignore-regexp "\\(?:\\`\\(?:\\.\\|__\\)\\|elc\\|pyc$\\)"))

;; Use swiper less, it takes up `ivy-height' lines.
(use-package isearch
  :ensure nil
  :bind (:map isearch-mode-map
         ("C-o" . swiper-from-isearch)
         ;; consistent with ivy-occur
         ("C-c C-o" . isearch-occur)
         ;; Edit the search string instead of jumping back
         ([remap isearch-delete-char] . isearch-del-char))
  :config
  (define-advice isearch-occur (:after (_regexp &optional _nlines))
    (isearch-exit))
  :custom
  ;; One space can represent a sequence of whitespaces
  (isearch-lax-whitespace t)
  (isearch-regexp-lax-whitespace t)
  (search-whitespace-regexp "[ \t\r\n]+")
  (isearch-allow-scroll t)
  (isearch-lazy-count t)
  (isearch-yank-on-move t)
  (lazy-count-prefix-format nil)
  (lazy-count-suffix-format " [%s/%s]")
  (lazy-highlight-buffer t)
  (lazy-highlight-cleanup nil))

;; isearch alternative
(use-package swiper
  :ensure t
  :custom
  (swiper-action-recenter t))

;; Writable grep buffer. company well with ivy-occur
(use-package wgrep
  :ensure t
  :defer 1
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

;; Pixel alignment for org/markdown tables
(use-package valign
  :ensure t
  :quelpa (valign :fetcher github :repo "casouri/valign")
  :hook ((markdown-mode org-mode) . valign-mode)
  :config
  ;; compatible with outline mode
  (define-advice outline-show-entry (:override nil)
    "Show the body directly following this heading.
Show the heading too, if it is currently invisible."
    (interactive)
    (save-excursion
      (outline-back-to-heading t)
      (outline-flag-region (max (point-min) (1- (point)))
                           (progn
                             (outline-next-preface)
                             (if (= 1 (- (point-max) (point)))
                                 (point-max)
                               (point)))
                           nil)))
  )

;; Jekyll blog posts manager
(use-package jblog
  :ensure t
  :quelpa (jblog :fetcher github :repo "condy0919/jblog")
  :commands jblog
  :config
  (evil-set-initial-state 'jblog-mode 'normal)
  (evil-collection-define-key 'normal 'jblog-mode-map
    "C" 'jblog-create
    "D" 'jblog-delete
    "O" 'jblog-open-posts-directory
    "s" 'jblog-search
    "gr" 'jblog-refresh
    "q" 'quit-window)
  :custom
  (jblog-posts-directory (expand-file-name "~/blog/_posts"))
  (jblog-post-headers [("Date"       12 t)
                       ("Title"      36 t)
                       ("Categories" 25 t)
                       ("Tags"       25 t)])
  (jblog-post-headers-format "---
layout: post
title: %s
categories: note
tags: note
usemermaid: false
usemathjax: false
---

* content
{:toc}
"))


;; The markdown mode is awesome! unbeatable
(use-package markdown-mode
  :ensure t
  :init
  (advice-add #'markdown--command-map-prompt :override #'ignore)
  (advice-add #'markdown--style-map-prompt   :override #'ignore)
  :mode ("README\\(?:\\.md\\)?\\'" . gfm-mode)
  :hook (markdown-mode . visual-line-mode)
  :bind (:map markdown-mode-style-map
         ("r" . markdown-insert-ruby-tag))
  :config
  (defun markdown-insert-ruby-tag (text ruby)
    "Quick insertion of ruby tag with `TEXT' and `RUBY'."
    (interactive "sText: \nsRuby: \n")
    (insert (format "<ruby>%s<rp>(</rp><rt>%s</rt><rp>)</rp></ruby>" text ruby)))

  (with-eval-after-load 'evil-collection
    (evil-collection-define-key 'normal 'markdown-mode-map
      (kbd "<tab>") 'markdown-cycle
      (kbd "S-<tab>") 'markdown-shifttab))
  :custom
  (markdown-header-scaling t)
  (markdown-enable-wiki-links t)
  (markdown-italic-underscore t)
  (markdown-asymmetric-header t)
  (markdown-gfm-uppercase-checkbox t)
  (markdown-fontify-code-blocks-natively t))

;; Generate table of contents for markdown-mode
(use-package markdown-toc
  :ensure t
  :after markdown-mode
  :bind (:map markdown-mode-command-map
         ("r" . markdown-toc-generate-or-refresh-toc)))

;; Free hands
(use-package auto-package-update
  :ensure t
  :custom
  (auto-package-update-delete-old-versions t))

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

;; Pastebin service
(use-package webpaste
  :ensure t
  :custom
  (webpaste-open-in-browser t)
  (webpaste-paste-confirmation t)
  (webpaste-add-to-killring nil)
  (webpaste-provider-priority '("paste.mozilla.org" "dpaste.org" "ix.io")))

;; Web search
(use-package webjump
  :ensure nil
  :bind ("C-c /" . webjump)
  :custom
  (webjump-sites '(
                   ;; Emacs.
                   ("Emacs Home Page" .
                    "www.gnu.org/software/emacs/emacs.html")
                   ("Savannah Emacs page" .
                    "savannah.gnu.org/projects/emacs")

                   ;; Internet search engines.
                   ("DuckDuckGo" .
                    [simple-query "duckduckgo.com"
                                  "duckduckgo.com/?q=" ""])
                   ("Google" .
                    [simple-query "www.google.com"
                                  "www.google.com/search?q=" ""])
                   ("Google Groups" .
                    [simple-query "groups.google.com"
                                  "groups.google.com/groups?q=" ""])
                   ("Wikipedia" .
                    [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""])

                   ;; ArchLinux
                   ("ArchLinux Packages" .
                    [simple-query "www.archlinux.org"
                                  "www.archlinux.org/packages/?q=" ""])
                   ("ArchLinux User Repository" .
                    [simple-query "aur.archlinux.org"
                                  "aur.archlinux.org/packages/?K=" ""]))))

;; Upload to 0x0.st or other pb services
(use-package 0x0
  :ensure t)

;; Edit text for browser with GhostText or AtomicChrome extension
(use-package atomic-chrome
  :ensure t
  :commands (evil-set-initial-state)
  :hook ((emacs-startup . atomic-chrome-start-server)
         (atomic-chrome-edit-mode . delete-other-windows))
  :config
  ;; The browser is in "insert" state, makes it consistent
  (evil-set-initial-state 'atomic-chrome-edit-mode 'insert)
  :custom
  (atomic-chrome-buffer-open-style 'frame)
  (atomic-chrome-default-major-mode 'markdown-mode)
  (atomic-chrome-url-major-mode-alist '(("github\\.com" . gfm-mode))))

;; Open very large files
(use-package vlf-setup
  :ensure vlf)

;; IRC client
(use-package rcirc
  :ensure nil
  :hook ((rcirc-mode . rcirc-track-minor-mode)
         (rcirc-mode . rcirc-omit-mode))
  :custom
  (rcirc-default-port 7000)
  (rcirc-always-use-server-buffer-flag t)
  (rcirc-authenticate-before-join t)
  (rcirc-auto-authenticate-flag t)
  (rcirc-fill-column #'frame-text-width)
  (rcirc-kill-channel-buffers t))

;; Quickly insert url
(use-package quickurl
  :ensure nil
  :bind ("C-c i q" . quickurl-prefix-map)
  :config
  (defvar quickurl-prefix-map (make-sparse-keymap))
  (define-prefix-command 'quickurl-prefix-map)
  (define-key quickurl-prefix-map "q" 'quickurl)
  (define-key quickurl-prefix-map "l" 'quickurl-list)
  (define-key quickurl-prefix-map "i" 'quickurl-ask)
  (define-key quickurl-prefix-map "e" 'quickurl-edit-urls)
  (define-key quickurl-prefix-map "a" 'quickurl-add-url)
  (define-key quickurl-prefix-map "b" 'quickurl-browse-url-ask)
  :custom
  (quickurl-format-function #'quickurl-url-url))

;; The builtin dictionary app in MacOS
(when (eq system-type 'darwin)
  (use-package osx-dictionary
    :ensure t
    :commands osx-dictionary-search-word-at-point))

(provide 'init-tools)

;;; init-tools.el ends here
