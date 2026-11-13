;;; init-utils.el --- Utilities -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defconst tldr-buffer-name "*tldr*")
(defconst tldr-url-template
  "https://raw.githubusercontent.com/tldr-pages/tldr/main/pages/%s/%s.md")
(defconst tldr-platforms
  '("common" "linux" "osx" "windows" "freebsd" "openbsd" "netbsd" "sunos" "dos" "android" "cisco-ios")
  "Supported platform directories in tldr-pages.")

(defvar url-http-end-of-headers)
(defvar url-http-response-status)
(declare-function url-hexify-string "url-util" (string &optional allowed-chars))
(declare-function markdown-mode "ext:markdown-mode" ())

(defun tldr--default-platform ()
  "Return the default tldr platform directory for `system-type'."
  (pcase system-type
    ((or 'gnu 'gnu/linux) "linux")
    ('darwin "osx")
    ((or 'windows-nt 'cygwin) "windows")
    ('ms-dos "dos")
    ('android "android")
    ('berkeley-unix "freebsd")
    (_ "common")))

(defun tldr--format-error (err http-status)
  "Return a concise error description for ERR and HTTP-STATUS."
  (pcase err
    (`(error http ,code) (format "HTTP %s" code))
    ((pred identity) (format "%S" err))
    (_ (format "HTTP %s" (or http-status "unknown")))))

(defun tldr--render (content)
  "Display tldr Markdown CONTENT in `tldr-buffer-name'."
  (let ((buf (get-buffer-create tldr-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert content)
        (goto-char (point-min)))
      (unless (derived-mode-p 'markdown-mode)
        (when (fboundp 'markdown-mode)
          (markdown-mode))))
    (pop-to-buffer buf)
    (goto-char (point-min))
    (view-mode-enter)))

(defun tldr--fetch (cmd platform &optional allow-fallback)
  "Fetch tldr page for CMD under PLATFORM asynchronously.
If ALLOW-FALLBACK is non-nil and the page is not found (HTTP 404),
retry once under \"common\"."
  (require 'url)
  (let ((url (format tldr-url-template
                     (url-hexify-string platform)
                     (url-hexify-string cmd))))
    (message "tldr: fetching %s (%s)..." cmd platform)
    (url-retrieve
     url
     (lambda (status)
       (let ((http-buf (current-buffer))
             (err (plist-get status :error))
             (http-status (bound-and-true-p url-http-response-status))
             (hdr-end (bound-and-true-p url-http-end-of-headers))
             content)
         (unwind-protect
             (when (and (not err) (eql http-status 200) (integer-or-marker-p hdr-end))
               (goto-char hdr-end)
               (skip-chars-forward "\r\n")
               (setq content (decode-coding-string
                              (buffer-substring-no-properties (point) (point-max))
                              'utf-8)))
           (when (buffer-live-p http-buf)
             (kill-buffer http-buf)))
         (cond
          (content
           (message nil)
           (tldr--render content))
          ((and allow-fallback
                (not (string= platform "common"))
                (eql http-status 404))
           (tldr--fetch cmd "common" nil))
          (t
           (message "tldr: failed to fetch %s (%s): %s"
                    cmd platform (tldr--format-error err http-status))))))
     nil t t)))

;;;###autoload
(defun tldr (cmd &optional platform)
  "View tldr page for CMD asynchronously.
When PLATFORM is nil, query the platform matching `system-type'
first and fall back to \"common\" if not found.  When PLATFORM is
non-nil (interactively via prefix argument), query only PLATFORM."
  (interactive
   (let* ((default (thing-at-point 'symbol t))
          (prompt (if default
                      (format "Command (default %s): " default)
                    "Command: "))
          (input (read-string prompt nil nil default)))
     (list input
           (when current-prefix-arg
             (completing-read "Platform: " tldr-platforms nil t)))))
  (let ((normalized (replace-regexp-in-string "[[:space:]]+" "-" (string-trim cmd))))
    (when (string= normalized "")
      (user-error "Command cannot be empty"))
    (tldr--fetch normalized (or platform (tldr--default-platform)) (null platform))))

(provide 'init-utils)
;;; init-utils.el ends here
