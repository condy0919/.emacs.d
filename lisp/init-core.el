;;; init-core.el --- core functions -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:

(eval-when-compile
  (require 'init-macros))

;;;###autoload
(defun my/rename-file (file)
  "Rename `FILE'. If the `FILE' is opened, rename the corresponding buffer too."
  (interactive)
  (let* ((dir (file-name-directory file))
         (new-name (read-file-name "New name: " dir nil 'confirm (file-name-nondirectory file)))
         (new-file (expand-file-name new-name dir)))
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
(defun my/copy-current-buffer-name (&rest _)
  "Copy the name to the current buffer."
  (interactive)
  (message (kill-new (buffer-name))))

;;;###autoload
(my/other-windowize-for eshell)

;;;###autoload
(my/other-windowize-for ansi-term shell-file-name)

;;;###autoload
(defun my/ansi-term (&rest _)
  "Open a `term' in current window."
  (interactive)
  (ansi-term shell-file-name))

;;;###autoload
(defun my/buffer-auto-close ()
  "Close buffer after exit."
  (when (ignore-errors (get-buffer-process (current-buffer)))
    (set-process-sentinel (get-buffer-process (current-buffer))
                          (lambda (process _exit-msg)
                            (when (memq (process-status process) '(exit stop))
                              (kill-buffer (process-buffer process))
                              (when (> (count-windows) 1)
                                (delete-window)))))))

;;;###autoload
(my/other-windowize-for ielm)

;;;###autoload
(defun my/transient-winner-undo ()
  "Transient version of `winner-undo'."
  (interactive)
  (let ((echo-keystrokes nil))
    (winner-undo)
    (message "Winner: [u]ndo [r]edo")
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map "u" #'winner-undo)
       (define-key map "r" #'winner-redo)
       map)
     t)))

;;;###autoload
(defun my/transient-tab-bar-undo-close-tab ()
  "Transient version of `tab-bar-undo-close-tab'."
  (interactive)
  (let ((echo-keystrokes nil))
    (tab-bar-undo-close-tab)
    (message "Tab: [u]ndo")
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map "u" #'tab-bar-undo-close-tab)
       map)
     t)))

;; Lock buffer to window
(define-minor-mode sticky-buffer-mode
  "Make the current window always display this buffer."
  nil " sticky" nil
  (set-window-dedicated-p (selected-window) sticky-buffer-mode))

;;;###autoload
(defun my/suppress-message (func &rest args)
  "Suppress `message'."
  (let ((inhibit-message t))
    (apply func args)))

(provide 'init-core)
;;; init-core.el ends here
