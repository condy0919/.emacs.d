;;; init-utils.el --- Utilities -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'url)
(require 'json)

(defconst tldr-buffer-name "*tldr*")
(defconst tldr-url-template "https://api.github.com/repos/tldr-pages/tldr/contents/pages/%s/%s.md")

;; Silence compile warnings
(defvar url-http-end-of-headers)

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
                      (goto-char url-http-end-of-headers)
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

(defconst wttr-default-cities '("北京" "上海" "深圳" "杭州"))
(defconst wttr-url-template "https://weathernew.pae.baidu.com/weathernew/pc?query=%s天气&srcid=4982")

;;;###autoload
(defun wttr (city)
  "Weather forecast for CITY."
  (interactive (list (completing-read "City: " wttr-default-cities)))
  (let ((url (format wttr-url-template city)))
    (xwidget-webkit-browse-url url)))

(provide 'init-utils)
;;; init-utils.el ends here
