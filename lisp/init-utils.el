;;; init-utils.el --- Utilities -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'url)
(require 'json)
(require 'thingatpt)

(defconst ydcv-buffer-name "*ydcv*")

;;;###autoload
(defun ydcv-dwim (word)
  "Call `ydcv' on WORD."
  (interactive (list (if (use-region-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (word-at-point))))
  (let ((max-mini-window-height 0)
        (buf (get-buffer-create ydcv-buffer-name)))
    (shell-command (concat "ydcv " (shell-quote-argument word)) buf)
    (with-current-buffer buf
      (view-mode +1)
      (pop-to-buffer buf))))

(defconst tldr-buffer-name "*tldr*")
(defconst tldr-url-template "https://api.github.com/repos/tldr-pages/tldr/contents/pages/%s/%s.md")

;;;###autoload
(defun tldr (cmd &optional op)
  "View tldr page of CMD.
If OP is non-nil and search failed, OP will be used as platform
name and search again. Typically OP is nil or \"common\"."
  (interactive "sCommand: ")
  (let* ((platform (or op
                     (pcase system-type
                       ('gnu "linux")
                       ('gnu/linux "linux")
                       ('darwin "osx")
                       ('ms-dos "windows"))))
         (url (format tldr-url-template platform cmd)))
    (url-retrieve url
                  (lambda (status)
                    (if (or (not status) (plist-member status :error))
                        (if (not op)
                            (tldr cmd "common")
                          (user-error "Something went wrong.\n\n%s" (pp-to-string (plist-get status :error))))
                      (search-forward "\n\n")
                      (let* ((req (json-read))
                             (encoding (alist-get 'encoding req))
                             (content (alist-get 'content req)))
                        (cl-assert (string= encoding "base64"))
                        (let ((buf (get-buffer-create tldr-buffer-name))
                              (inhibit-read-only t))
                          (with-current-buffer buf
                            (erase-buffer)
                            (insert (base64-decode-string content))
                            (when (functionp 'markdown-mode)
                              (markdown-mode))
                            (view-mode +1)
                            (pop-to-buffer buf)))))))))

(provide 'init-utils)
;;; init-utils.el ends here
