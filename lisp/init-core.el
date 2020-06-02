;;; init-core.el --- core functions -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:

;;;###autoload
(defun my/rename-file (file)
  "Rename `FILE'. If the `FILE' is opened, rename the corresponding buffer too."
  (interactive)
  (let* ((new-name (read-string "NewName: "))
         (old-dir (file-name-directory file))
         (new-file (concat old-dir new-name)))
    (rename-file file new-file)
    (when-let* ((buf (find-buffer-visiting file)))
      (if (string= file (buffer-file-name))
          (progn
            (set-visited-file-name new-file)
            (rename-buffer new-file))
        (kill-buffer buf)
        (find-file-noselect new-file)))))

;;;###autoload
(defun my/rename-current-file (&rest _)
  "Rename current visiting file."
  (interactive)
  (or (buffer-file-name) (error "No file is visiting"))
  (my/rename-file (buffer-file-name)))

;;;###autoload
(defun my/delete-file (file)
  "Delete `FILE'. If the `FILE' is opened, kill the corresponding buffer too."
  (interactive)
  (when (y-or-n-p (format "Really delete '%s'? " file))
    (when-let* ((buf (find-buffer-visiting file)))
      (kill-buffer buf))
    (delete-file file)))

;;;###autoload
(defun my/delete-current-file (&rest _)
  "Delete current visiting file, and kill the corresponding buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is visiting"))
  (my/delete-file (buffer-file-name)))

;;;###autoload
(defun my/copy-current-file (new-path &optional overwrite-p)
  "Copy current buffer's file to `NEW-PATH'.
If `OVERWRITE-P', overwrite the destination file without
confirmation."
  (interactive (list (read-file-name "Copy file to: ")
                     current-prefix-arg))
  (or (buffer-file-name) (error "No file is visiting"))
  (let ((old-path (buffer-file-name))
        (new-path (expand-file-name new-path)))
    (make-directory (file-name-directory new-path) 't)
    (copy-file old-path new-path (or overwrite-p 1))))

;;;###autoload
(defun my/copy-current-filename (&rest _)
  "Copy the full path to the current file."
  (interactive)
  (or (buffer-file-name) (error "No file is visiting"))
  (message (kill-new (buffer-file-name))))

;;;###autoload
(defun my/eshell-other-window (&rest _)
  "Open a `eshell' in a new window."
  (interactive)
  (let ((buf (eshell)))
    (switch-to-buffer (other-buffer buf))
    (switch-to-buffer-other-window buf)))

;;;###autoload
(defun my/term-other-window (&rest _)
  "Open a `term' in a new window."
  (interactive)
  (let ((buf (ansi-term shell-file-name)))
    (switch-to-buffer (other-buffer buf))
    (switch-to-buffer-other-window buf)))

;;;###autoload
(defun my/term (&rest _)
  "Open a `term' in current window."
  (interactive)
  (ansi-term shell-file-name))

;;;###autoload
(defun my/buffer-auto-close ()
  "Close buffer after exit."
  (when (ignore-errors (get-buffer-process (current-buffer)))
    (set-process-sentinel (get-buffer-process (current-buffer))
                          (lambda (process exit-msg)
                            (when (string-match "\\(finished\\|exited\\)" exit-msg)
                              (kill-buffer (process-buffer process))
                              (when (> (count-windows) 1)
                                (delete-window)))))))

(provide 'init-core)
;;; init-core.el ends here
