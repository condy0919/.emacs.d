;;; init-utils.el --- Utilities -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defconst ydcv-buffer-name "*ydcv*")

;;;###autoload
(defun ydcv-dwim ()
  "Call `ydcv' with active region or current symbol."
  (interactive)
  (let ((max-mini-window-height 0))
    (if (use-region-p)
        (shell-command-on-region (region-beginning) (region-end) "ydcv" ydcv-buffer-name)
      (shell-command (format "ydcv %s" (word-at-point)) ydcv-buffer-name))
    (with-current-buffer ydcv-buffer-name
      (view-mode +1))))

;;;###autoload
(defun qrencode-on-region (start end)
  "Call `qrencode' from START to END."
  (interactive "r")
  (let ((buf (get-buffer-create "*QRCode*"))
        (inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer))
    (let ((coding-system-for-read 'raw-text))
      (shell-command-on-region start end "qrencode -o -" buf))
    (switch-to-buffer buf)
    (image-mode)))

(provide 'init-utils)

;;; init-utils.el ends here
