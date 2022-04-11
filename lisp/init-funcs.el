;;; init-funcs.el --- core functions -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:

;;;###autoload
(defun +rename-current-file (newname)
  "Rename current visiting file to NEWNAME.
If NEWNAME is a directory, move file to it."
  (interactive
   (progn
     (unless buffer-file-name
       (user-error "No file is visiting"))
     (let ((name (read-file-name "Rename to: " nil buffer-file-name 'confirm)))
       (when (equal (file-truename name)
                    (file-truename buffer-file-name))
         (user-error "Can't rename file to itself"))
       (list name))))
  ;; NEWNAME is a directory
  (when (equal newname (file-name-as-directory newname))
    (setq newname (concat newname (file-name-nondirectory buffer-file-name))))
  (rename-file buffer-file-name newname)
  (set-visited-file-name newname)
  (rename-buffer newname))

;;;###autoload
(defun +delete-current-file (file)
  "Delete current visiting FILE."
  (interactive
   (list (or buffer-file-name
             (user-error "No file is visiting"))))
  (when (y-or-n-p (format "Really delete '%s'? " file))
    (kill-this-buffer)
    (delete-file file)))

;;;###autoload
(defun +copy-current-file (new-path &optional overwrite-p)
  "Copy current buffer's file to `NEW-PATH'.
If `OVERWRITE-P', overwrite the destination file without
confirmation."
  (interactive
   (progn
     (unless buffer-file-name
       (user-error "No file is visiting"))
     (list (read-file-name "Copy file to: ")
           current-prefix-arg)))
  (let ((old-path (buffer-file-name))
        (new-path (expand-file-name new-path)))
    (make-directory (file-name-directory new-path) t)
    (copy-file old-path new-path (or overwrite-p 1))))

;;;###autoload
(defun +copy-current-filename (file)
  "Copy the full path to the current FILE."
  (interactive
   (list (or buffer-file-name
             (user-error "No file is visiting"))))
  (kill-new file)
  (message "Copying '%s' to clipboard" file))

;;;###autoload
(defun +copy-current-buffer-name ()
  "Copy the name of current buffer."
  (interactive)
  (kill-new (buffer-name))
  (message "Copying '%s' to clipboard" (buffer-name)))

;;;###autoload
(defun +transient-tab-bar-history ()
  "Transient map of command `tab-bar-history'."
  (interactive)
  (let ((echo-keystrokes nil))
    (tab-bar-history-back)
    (message "tab-bar-history: [u]back [r]forward")
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map "u" #'tab-bar-history-back)
       (define-key map "r" #'tab-bar-history-forward)
       map)
     t)))

(provide 'init-funcs)
;;; init-funcs.el ends here
