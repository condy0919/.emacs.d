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
    (shell-command (format "ydcv '%s'" word) buf)
    (with-current-buffer buf
      (view-mode +1)
      (pop-to-buffer buf))))

(defconst tldr-buffer-name "*tldr*")
(defconst tldr-url-template "https://api.github.com/repos/tldr-pages/tldr/contents/pages/common/%s.md")

;;;###autoload
(defun tldr (cmd)
  "View tldr page for CMD."
  (interactive "sCommand: ")
  (let ((url (format tldr-url-template cmd)))
    (url-retrieve url
                  (lambda (status)
                    (if (or (not status) (plist-member status :error))
                        (user-error "Something went wrong.\n\n%s" (pp-to-string (plist-get status :error)))
                      (search-forward "\n\n")
                      (let* ((req (json-read))
                             (encoding (alist-get 'encoding req))
                             (content (alist-get 'content req)))
                        (cl-assert (string= encoding "base64"))
                        (let ((buf (get-buffer-create tldr-buffer-name)))
                          (with-current-buffer buf
                            (insert (base64-decode-string content))
                            (when (functionp 'markdown-mode)
                              (markdown-mode))
                            (view-mode +1)
                            (pop-to-buffer buf)))))))))

(provide 'init-utils)
;;; init-utils.el ends here
