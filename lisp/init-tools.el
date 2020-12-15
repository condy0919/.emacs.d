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
    "C-x n" "narrow")
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

  ;; Additional key bindings for ivy
  (use-package ivy-hydra
    :ensure t)

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
  :bind (([remap evil-show-registers] . counsel-evil-registers)
         ([remap evil-show-marks]     . counsel-evil-marks)
         ([remap evil-show-jumps]     . evil-jump-list)
         ([remap recentf-open-files]  . counsel-recentf)
         ([remap swiper]              . counsel-grep-or-swiper))
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
  (counsel-yank-pop-separator "\n───────────\n")
  (counsel-find-file-at-point t)
  (counsel-find-file-ignore-regexp "\\(?:\\`\\(?:\\.\\|__\\)\\|elc\\|pyc$\\)"))

;; Use swiper less, it takes up `ivy-height' lines.
(use-package isearch
  :ensure nil
  :bind (:map isearch-mode-map
         ("M-<return>"                . isearch-repeat-forward)
         ("M-S-<return>"              . isearch-repeat-backward)
         ("C-o"                       . swiper-from-isearch)
         ;; consistent with ivy-occur
         ("C-c C-o"                   . isearch-occur)
         ([escape]                    . isearch-cancel)
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
  (webjump-sites '(;; Internet search engines.
                   ("DogeDoge" .
                    [simple-query "dogedoge.com"
                                  "dogedoge.com/results?q=" ""])
                   ("Google" .
                    [simple-query "www.google.com"
                                  "www.google.com/search?q=" ""])
                   ("Wikipedia" .
                    [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""])
                   ("Urban Dictionary" .
                    [simple-query "urbandictionary.com" "www.urbandictionary.com/define.php?term=" ""])
                   ("Ludwig Guru" .
                    [simple-query "ludwig.guru" "ludwig.guru/s/" ""])
                   ("Etymology" .
                    [simple-query "etymonline.com" "etymonline.com/word/" ""])
                   ("Stack Overflow" .
                    [simple-query "stackoverflow.com" "stackoverflow.com/search?q=" ""])
                   ("GitHub" .
                    [simple-query "github.com" "github.com/search?ref=simplesearch&q=" ""])
                   ("TLDR" .
                    [simple-query "linux.cn" "tldr.linux.cn/cmd/" ""])

                   ;; Language specific engines.
                   ("Python Docs" .
                    [simple-query "docs.python.org"
                                  "docs.python.org/3/search.html?q=" ""])
                   ("Cpp Reference" .
                    [simple-query "en.cppreference.com"
                                  "en.cppreference.com/mwiki/index.php?search=" ""])

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
  :when (display-graphic-p)
  :hook ((emacs-startup . atomic-chrome-start-server)
         (atomic-chrome-edit-mode . delete-other-windows))
  :config
  ;; The browser is in "insert" state, makes it consistent
  (with-eval-after-load 'evil
    (evil-set-initial-state 'atomic-chrome-edit-mode 'insert))
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
  :config
  (add-to-list 'yank-excluded-properties 'rcirc-text)
  :custom
  (rcirc-default-port 7000)
  (rcirc-always-use-server-buffer-flag t)
  (rcirc-authenticate-before-join t)
  (rcirc-auto-authenticate-flag t)
  (rcirc-fill-column #'frame-text-width)
  (rcirc-kill-channel-buffers t))

;; Client to dict server
(use-package dictionary
  :ensure nil
  :when (>= emacs-major-version 28)
  :commands dictionary-search
  :custom
  (dictionary-default-popup-strategy "lev")
  (dictionary-server "dict.org"))

(provide 'init-tools)

;;; init-tools.el ends here
