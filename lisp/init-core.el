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

(provide 'init-core)
;;; init-core.el ends here
