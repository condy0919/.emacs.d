;;; init-ivy.el --- Incremental completion system and other tools -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; Writable grep buffer
;;
;; `ivy-wgrep-change-to-wgrep-mode' is an alias to it
(use-package wgrep
  :ensure t
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

;; Incremental vertical completion system
;;
;; faster than the builtin `icomplete-mode'
(use-package ivy
  :ensure t
  :hook (after-init . ivy-mode)
  :bind (:map ivy-minibuffer-map
         ("C-c C-e" . ivy-woccur)
         ("C-w"     . ivy-yank-word)
         :map ivy-occur-mode-map
         ("C-c C-e" . ivy-wgrep-change-to-wgrep-mode)
         :map ivy-occur-grep-mode-map
         ("C-c C-e" . ivy-wgrep-change-to-wgrep-mode))
  :config
  (with-no-warnings
    (defun ivy-woccur ()
      "ivy-occur with wgrep-mode enabled."
      (interactive)
      (run-with-idle-timer 0 nil 'ivy-wgrep-change-to-wgrep-mode)
      (ivy-occur)))
  :custom
  (ivy-height 10)                      ;; 10 candidates
  (ivy-fixed-height-minibuffer t)      ;; fixed height
  (ivy-display-style 'fancy)           ;; fancy style
  (ivy-count-format "%d/%d ")          ;; better counts
  (ivy-use-virtual-buffers t)          ;; show recent files
  (ivy-sort-max-size 3000)             ;; the default value 30000 is too large
  (ivy-on-del-error-function 'ignore)) ;; dont quit minibuffer when del-error

(use-package counsel
  :ensure t
  :hook (after-init . counsel-mode)
  :bind (([remap evil-show-registers] . counsel-evil-registers)
         ([remap evil-show-marks]     . counsel-evil-marks)
         ([remap evil-show-jumps]     . counsel-evil-jumps)
         ([remap recentf-open-files]  . counsel-recentf))
  :config
  (defun counsel-evil-jumps ()
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
                            (goto-char (marker-position mark))))))))

(use-package swiper
  :ensure t
  :bind (:map isearch-mode-map
         ("C-o" . swiper-from-isearch))
  :custom
  (swiper-action-recenter t))

(use-package counsel-projectile
  :ensure t
  :hook (after-init . counsel-projectile-mode))

(provide 'init-ivy)
;;; init-ivy.el ends here
